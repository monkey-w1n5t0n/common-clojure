---
stability: aspirational
layer: behavioural
audience: both
---

# Library Adaptation

> Spec: the behavioral milestones proving that useful Clojure libraries can be adapted to native Common Clojure without JVM compatibility or compiler backdoors. Counterpart to [MAIN.md](MAIN.md).
> See also [language-and-compatibility.md](language-and-compatibility.md) and [common-lisp-interop.md](common-lisp-interop.md).

## Source files

- `clojure-tests/` — reference idioms and core behavior used by candidate adaptations.
- `cl-clojure-eval.lisp` — legacy hand-written namespace stubs that do not count as adaptations.

No target library source is currently vendored; versioned adaptation sources must be identified when each milestone begins.

---

## 1. Frame

1.1 Library adaptation demonstrates that Common Clojure is useful for new projects; it does not promise unchanged Maven artifacts or JVM internals.

**Why:** The selected libraries exercise the language abstractions the user values while allowing principled host-specific replacement.

1.2 An adaptation MAY modify source, replace Java/JVM internals with CL adapters, rename host-specific namespaces, or reduce peripheral features, but MUST document every behavioral cut exercised by its public examples.

1.3 Success means representative library programs compile to native artifacts, execute their assertions, and contain no runtime evaluator dependency. Merely defining stub names or loading files is failure.

---

## 2. Shared adaptation contract

2.1 Adapted library code uses ordinary Common Clojure macros, functions, Vars, persistent collections, protocols, multimethods, tagged literals, and declared CL bridges. It MUST NOT access private compiler representation.

2.2 JVM class checks, reflection, bytecode emitters, thread primitives, and file/resource assumptions are replaced by explicit Common Lisp/SBCL adapters with focused tests.

2.3 Every milestone records the upstream library/version or semantic source, adapted namespaces, unsupported features, bridge modules, and conformance examples.

2.4 Macro-generated runtime code follows the same analyzer and native compilation pipeline as written source.

2.5 Failures use the language condition and source-location model; an adaptation MUST NOT swallow a compiler error and choose an interpreted implementation.

---

## 3. core.match milestone

3.1 The core.match adaptation MUST implement a representative match macro that compiles literal, wildcard, binding, sequence/vector, map-key, predicate/guard, alternation, and fall-through cases into native decision code.

3.2 Pattern expressions and guards obey left-to-right single evaluation where the source contract requires it; failed alternatives do not leak bindings.

3.3 Exhaustive successful, no-match/default, guard-failure, and malformed-pattern examples MUST run identically in interactive and AOT modes.

3.4 Generated match execution MUST not retain pattern ASTs for runtime walking.

**Why:** core.match is the clearest proof that sophisticated macro-defined syntax can compile through the private analyzer to efficient native control flow.

---

## 4. spec.alpha milestone

4.1 The spec.alpha adaptation MUST support named specs, predicate specs, conjunction/disjunction, nilability, collection element specs, map-key specs, conform/unform where defined, validity checks, and structured explain data for a representative domain model.

4.2 Spec registry entries are namespace-qualified and redefine coherently through Vars/session state. Predicate and conformer functions are native compiled callables.

4.3 Explain data is persistent Common Clojure data with source-meaningful paths and causes; validation failure is not conflated with nil or false input.

4.4 Generative testing integration MAY be adapted separately; absence of JVM test.check integration MUST be documented and MUST NOT weaken deterministic validation claims.

**Why:** spec.alpha exercises functions-as-data, registries, macros, persistent nested data, explainability, and nil/false distinctions.

---

## 5. O'Doyle Rules milestone

5.1 The O'Doyle Rules adaptation MUST support declaring rules, creating a session, inserting and retracting facts, firing until quiescence, querying derived state, and preserving deterministic results for a representative rules program.

5.2 Rule left-hand matching and right-hand actions compile to native functions. The runtime MAY use mutable indexed state internally when session semantics expose controlled mutation, but published persistent values remain immutable.

5.3 Rule identity, activation order where public, repeated insertion, retraction, and update behavior MUST be pinned by focused examples rather than inferred from incidental host map iteration order.

5.4 The milestone MUST include a nontrivial program with multiple interacting rules and demonstrate that no evaluator walks rule source forms at runtime.

**Why:** A rules engine tests data-oriented program construction, macros, indexing, protocols, and realistic application throughput.

---

## 6. core.async milestone

6.1 The core.async adaptation MUST support buffered and unbuffered channels, blocking or explicitly named thread operations, asynchronous put/take, close, selection among operations, and a `go`-style construct for representative pipelines.

6.2 A `go` body compiles at macro expansion into a native resumable state machine or continuation representation; parked execution MUST NOT resume by interpreting source forms.

6.3 Put/take ordering, close behavior, pending-operation completion, buffer behavior, and exception propagation MUST be pinned under deterministic tests before throughput claims.

6.4 Dynamic Var bindings and cleanup required across a parked/resumed computation MUST be captured or restored explicitly; accidental dependence on the worker thread's ambient specials is non-conforming.

6.5 SBCL threads, locks, condition variables, and timers MAY replace JVM executors and atomics through small host adapters.

**Why:** core.async is the hardest named milestone and proves that macro compilation plus native SBCL concurrency can support novel Clojure-style control abstractions.

---

## 7. Milestone reporting

7.1 Each library milestone reports passed behavioral examples, excluded upstream surfaces, native artifact evidence, performance measurements relevant to its workload, and remaining semantic questions.

7.2 Completion of one milestone does not imply general JVM library compatibility or completion of every upstream namespace.

---

## Open / Deferred

- **Which exact upstream versions seed each adaptation?** Choose at milestone start and record patches against that source.
- **How much of spec.alpha generation and instrumentation belongs in its first milestone?** Deterministic conformance is required; broader tooling should follow demonstrated use.
- **Should core.async guarantee compatibility with upstream scheduling fairness?** First characterize the observable contract independently of JVM executor accidents.
