# Alignment

**Last reviewed:** 2026-07-17

## Mission

Common Clojure exists to make Clojure's data-oriented, functional, macro-extensible
semantics feel native on SBCL: source compiles through Common Lisp into inspectable,
debuggable, high-performance SBCL artifacts, while selected libraries are adapted at
explicit platform seams. Success is not JVM emulation or evaluator-era test counts; it is
a coherent language in which new applications can be developed without falling out of
the Clojure model or forfeiting SBCL's infrastructure.

## Top defects

### 1. The only implementation on main executes by tree walking

**Dated:** 2026-07-17
**What:** The shipped ASDF system exposes `cl-clojure-eval`, explicit environments, and
closures that retain source bodies. There is no conforming reader → analyzer → emitter →
SBCL compiler path. See [the legacy assessment](docs/legacy-assessment.md).
**Why it blocks the mission:** Runtime behavior and performance are structurally
incapable of satisfying the native-artifact contract. Extending the evaluator deepens the
wrong architecture.
**Rough cost:** Extra large — a new compiler/runtime path and a deliberate removal gate,
not an incremental evaluator refactor.

### 2. The current form and value representations violate basic Clojure semantics

**Dated:** 2026-07-17
**What:** The reader conflates false with nil, treats map forms as mutable CL hash
tables, synthesizes metadata calls, and lacks the source model required for stable
diagnostics. Persistent values, equality, hashing, symbols, and keywords have no coherent
runtime ABI.
**Why it blocks the mission:** Every analyzer, macro, namespace, collection, and library
port would otherwise bake in identities that later have to be broken. Near-native code
is irrelevant if it computes the wrong language.
**Rough cost:** Large — settle the value ABI early, then build persistent implementations
behind it.

### 3. There is no trustworthy conformance or native-performance signal

**Dated:** 2026-07-17
**What:** The legacy harness counts files that load without a serious condition; its
`deftest` and `is` behavior can let false assertions survive. There is no isolated FASL
test, evaluator-dependency check, allocation measurement, disassembly check, or controlled
SBCL comparison benchmark.
**Why it blocks the mission:** Agents cannot distinguish semantic progress from silent
stubbing, nor native compilation from a disguised interpreter. A swarm would amplify
false positives.
**Rough cost:** Medium — a small honest harness first, then incremental semantic and
performance suites.

### 4. Compile-time state and module behavior do not exist

**Dated:** 2026-07-17
**What:** Semantic namespaces, Vars, ordered macro staging, compilation-session
transactions, runtime ABI checks, source-located phase conditions, ASDF `.clj` components,
and compiler-free artifact loading are all absent from main.
**Why it blocks the mission:** A native expression compiler alone cannot compile real
modules, define a macro and use it later in the same file, support redefinition, or serve
both a REPL and build system coherently.
**Rough cost:** Extra large — these concerns form the deep compiler seam and must be
designed together even when implemented in slices.

### 5. The legacy implementation concentrates unrelated semantics in one 9,901-line file

**Dated:** 2026-07-17
**What:** Special forms, collections, namespaces, testing stand-ins, interop, concurrency,
and thousands of core functions share one evaluator module and global state.
**Why it blocks the mission:** It provides no safe ownership seams for parallel agents;
local changes have broad, hidden effects, and extracting it piece by piece risks retaining
the evaluator as an accidental second source of truth.
**Rough cost:** Medium after the native vertical slice — quarantine immediately, mine
individual algorithms deliberately, and delete the shipped evaluator path once replacement
evidence exists.

### 6. The desired library surface is architectural intent, not demonstrated compatibility

**Dated:** 2026-07-17
**What:** `core.match`, `clojure.spec.alpha`, O'Doyle Rules, and `core.async` each depend on
different combinations of macros, persistent values, Vars, dispatch, metadata, and
concurrency. None has a native Common Clojure adaptation yet.
**Why it blocks the mission:** These libraries are the proof that the language is useful
for the intended style of application and that its extension seams are deep enough.
Without ports, compatibility claims remain theoretical.
**Rough cost:** Large and staged — macro-first evidence early; concurrency/state-machine
work only after the core runtime is sound.

## Open mission questions

- **2026-07-17 — Release threshold:** Which combination of core semantics and adapted
  libraries is sufficient to call the language usable rather than an architecture
  prototype?
- **2026-07-17 — Source compatibility:** How much namespace/API compatibility should
  adapted libraries preserve when an explicit `:common-clojure` or `:sbcl` branch would
  make the implementation clearer?
- **2026-07-17 — `core.async` scope:** Must an initial `go` implementation preserve the
  full observable parking/state-machine model, or may a documented native scheduler
  profile define a smaller first contract?
- **2026-07-17 — Sealed code:** What user-facing declaration and module boundary should
  opt into redefinition-breaking direct linkage once the development semantics are
  proven?

## Deferred / accepted debt

- **2026-07-17 — Other Common Lisps:** Portability beyond SBCL is deferred. Direct SBCL
  compiler, condition, threading, profiling, and disassembly integration is intentional.
- **2026-07-17 — JVM compatibility:** Java bytecode, unchanged Java interop, and arbitrary
  JVM Clojure projects are outside the mission.
- **2026-07-17 — Compiler plugins:** A public IR or compiler-plugin ABI is deferred until
  at least two real library ports demonstrate the same need.
- **2026-07-17 — Collection optimization:** Correct immutable bootstrap collections may
  precede vector tries and HAMTs, provided representation remains private and performance
  claims are not made until the optimized implementation passes its gate.
- **2026-07-17 — Generative spec tooling:** `clojure.spec.alpha` validation, conformance,
  and explanation may precede generators and `test.check` integration.
