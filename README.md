# Common Clojure

Common Clojure is a Clojure-semantic language hosted natively on Steel Bank Common
Lisp. Clojure source is read, macroexpanded, analyzed, lowered to Common Lisp, and
compiled by SBCL into ordinary native functions and FASLs.

The project is aimed at building new applications in a Clojure-shaped language while
using SBCL's compiler, runtime, debugger, profiler, object system, and library ecosystem.
Selected Clojure libraries may be ported with explicit platform adaptations.

## Contract

Runtime execution must not walk Clojure forms, retain evaluator environments, or fall
back to the legacy `clojure-eval`. Walking forms during compilation is expected; the
resulting program is native SBCL code.

The intended pipeline is:

```text
.clj source -> reader/forms -> macroexpansion -> semantic analysis/private IR
            -> Common Lisp emission -> SBCL compile/compile-file -> FASL/native code
```

Common Clojure targets Clojure's useful language semantics, not unchanged execution of
arbitrary JVM projects. SBCL is the deliberate host and sole compiler backend. Common
Lisp portability, Java emulation, and blanket compatibility with the official Clojure
test suite are not current goals.

## Current state

The checked-in implementation predates this direction. It contains a capable syntax
experiment and a large tree-walking evaluator, but no conforming native compiler yet.
Its tests and the vendored Clojure corpus are useful for characterization and semantic
examples; their historical pass counts are not evidence that the new contract is met.

The next milestone is a native vertical slice: compile a `.clj` module containing a
user macro and a typed `loop`/`recur`, produce a loadable FASL, call its exported
compiled function directly from Common Lisp, and demonstrate bounded-stack,
allocation-free loop execution without any evaluator dependency.

## Start here

- [MAP.md](MAP.md) is the terse repository map and spec-discovery entry.
- [docs/specs/MAIN.md](docs/specs/MAIN.md) is the normative behavioral contract.
- [docs/domain.md](docs/domain.md) defines the project's shared vocabulary.
- [docs/decisions/](docs/decisions/) records accepted architectural choices and rejected alternatives.
- [ALIGNMENT.md](ALIGNMENT.md) is the dated critique of gaps between the current code and the mission.
- [docs/swarm.md](docs/swarm.md) explains how multiple agents coordinate without creating parallel architectures.

## Working on the project

Coding work is tracked in Ergo. Use native-compilation epic `f86d60db` described in
[docs/swarm.md](docs/swarm.md); the imported evaluator-era backlog is intentionally not
the default work queue.

```bash
ergo ready --json
ergo list --status open --part-of f86d60db --json
ergo show <matching-id>
ergo claim <matching-id>
# implement and verify the task's cited spec clauses
ergo done <matching-id> --reason "Implemented and verified against the cited specs"
```

Choose only an ID present in both views; the global ready queue contains work from other
projects. Never claim the epic itself or use `--all` for this workstream.

`./tests.sh` exercises the legacy evaluator and is retained only as a characterization
signal during migration. Native compiler tasks must add acceptance tests at the public
compiler/artifact seam and keep the specs, map, and related decisions synchronized in
the same change.

## License

MIT
