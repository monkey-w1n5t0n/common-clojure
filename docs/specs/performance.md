---
stability: aspirational
layer: cross-cutting
audience: both
---

# Performance

> Spec: native-performance tiers, measurable gates, measurement controls, and the semantic costs Common Clojure permits. Counterpart to [MAIN.md](MAIN.md).
> See also [compiler-and-artifacts.md](compiler-and-artifacts.md), [functions-and-control-flow.md](functions-and-control-flow.md), and [collections-and-sequences.md](collections-and-sequences.md).

## Source files

- `sbcl-init.lisp` — existing SBCL optimization and compiler-note experiment.
- `cl-clojure-eval.lisp` — legacy evaluator whose runtime overhead is outside the target architecture.
- `cl-clojure-transducers.lisp` — legacy eager allocation evidence.
- `tests.sh` — current test entry point; not yet a performance harness.

---

## 1. Frame

1.1 Common Clojure aims for ordinary SBCL-native performance, with overhead only where a named Clojure semantic requirement needs dynamic behavior or persistent data.

**Why:** “Native” is meaningful only when hot code is measured against comparable hand-written Common Lisp rather than merely emitted into a compiled wrapper.

1.2 Performance optimization MUST NOT conflate nil and false, mutate persistent values, bypass dynamic Vars, stale protocol/multimethod dispatch, weaken failure behavior, or introduce an interpreter fallback.

---

## 2. Performance tiers

2.1 **Tier A — statically informed native code:** typed locals, resolved non-dynamic calls, sealed/final calls where asserted, and native control flow SHOULD approach equivalent declared Common Lisp.

2.2 **Tier B — ordinary Common Clojure:** resolved but redefinable Vars, generic numeric operations, persistent collections, and unsealed calls MAY pay the checks or indirection needed for their semantics.

2.3 **Tier C — explicitly dynamic code:** dynamic Vars, unknown callable values, protocols, multimethods, reflection-like host bridges, and language-level eval MAY pay dispatch or compilation costs that remain attributable and measurable.

2.4 A compiler optimization report SHOULD identify why a hot operation remains in Tier B or C and which safe declaration or program fact would move it to a lower-overhead tier.

**Why:** Explainable costs let authors tune code without guessing or abandoning semantics.

---

## 3. Initial native loop gate

3.1 The canonical loop benchmark is a compiled function performing 10,000,000 typed fixnum iterations through `loop*`/`recur`, using only fixnum decrement, comparison, and an accumulator operation that remains within fixnum range. After entry, its steady-state loop MUST use bounded stack and report zero bytes consed per iteration.

**Why:** This directly detects evaluator frames, closure-body walking, accidental recursion, boxing, and recur allocation.

3.2 The initial, low-confidence throughput gate is a median elapsed time no worse than 2.0 times an equivalent hand-written Common Lisp function carrying the same type and optimization declarations.

**Why:** A concrete ratio makes “close to native” falsifiable while leaving room for the truthiness and safety checks still present in the first compiler.

3.3 Measurement uses one SBCL build and process, `(speed 3) (safety 1) (debug 0)`, equivalent declarations, two unmeasured warm-up runs, seven measured runs, a full GC before each measured pair, alternating implementation order, and median paired ratios. CPU model, SBCL version, governor/pinning state, and raw samples MUST be reported.

3.4 Compiler setup, reading, macro expansion, and first invocation are excluded from the runtime ratio and reported separately as compilation latency.

3.5 Conformance also requires `compiled-function-p` for the benchmark function and inspection evidence of a native loop back-edge with no call into the legacy evaluator.

---

## 4. Collection and dispatch gates

4.1 Persistent vector append and indexed association MUST avoid whole-vector copying at scale and have logarithmic-or-better update depth with structural sharing. Persistent hash map/set updates MUST have expected logarithmic-or-better lookup/update behavior under ordinary hash distribution.

4.2 Building a large collection through the documented transient/builder path SHOULD be linear in element count and MUST NOT retain intermediate persistent versions unintentionally.

4.3 Lazy sequence production MUST remain bounded in work and retained storage per demanded element when the consumer does not retain the prefix.

4.4 Protocol and multimethod benchmarks MUST separately report cold dispatch, warm-cache dispatch, cache invalidation, and native method-body time; a single blended number is insufficient.

---

## 5. Build and artifact performance

5.1 File compilation and interactive form compilation MUST be measured separately from execution. No maximum latency is committed until the first vertical slice supplies a baseline.

5.2 Loading an application FASL MUST NOT initialize the compiler or reader and SHOULD do no work proportional to source AST size.

5.3 Performance regressions are evaluated per tier and benchmark rather than hidden in an aggregate suite duration.

---

## 6. Permitted semantic costs

6.1 Truthiness testing is permitted because nil and false are distinct falsey values.

6.2 Var indirection is permitted where live redefinition, first-class Vars, or dynamic binding requires it; ordinary non-dynamic function calls SHOULD use coherent native fdefinitions as specified by [namespaces-vars-and-macros.md §3.3](namespaces-vars-and-macros.md).

6.3 Protocol and multimethod dispatch cost is permitted because open extension and arbitrary dispatch are named semantics. Method bodies themselves remain native compiled code.

6.4 Persistent collection structure and hashing cost is permitted because immutability, structural sharing, and value equality are named semantics; full-copy updates are not.

---

## Open / Deferred

- **Is the initial 2.0× typed-loop threshold appropriately strict?** Confidence is low until the same harness measures the first compiler; changing it requires published samples, not intuition.
- **What compilation-latency budget keeps interactive development comfortable?** Establish after measuring cold and warm sessions on the operator's machines.
- **Which application-level benchmark should supplement microbenchmarks?** Choose from the first substantial rules, spec, match, or async program.
