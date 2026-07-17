# All execution surfaces share one compiler path

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

Ahead-of-time builds, interactive evaluation, ASDF integration, and Common Lisp embedding have different orchestration needs. Giving any surface its own evaluator or source translator would duplicate semantic behavior and allow code to work differently in development, builds, and embedded use.

## Decision

All execution surfaces enter the same deep compiler module and use the same reader, macroexpansion, staging, semantic analysis, private semantic IR, lowering, and runtime ABI.

- Ahead-of-time compilation writes a loadable native artifact.
- Interactive evaluation compiles the submitted forms as an in-memory native thunk and invokes it.
- The ASDF adapter delegates compilation to the same compiler seam.
- Common Lisp embedding loads artifacts and resolves or invokes Vars through the runtime ABI; it does not bypass semantic analysis for Clojure source.

Surface adapters may manage paths, dependency ordering, output locations, or presentation. They do not own language semantics. Exact public names and argument conventions remain implementation choices.

## Alternatives considered

- Keep the tree walker for the REPL and use the compiler for files. This creates immediate development-versus-production semantic drift.
- Give ASDF a separate source-to-CL transpiler. Build behavior would diverge from interactive and direct compiler use.
- Implement Clojure as a collection of Common Lisp macros for embedded forms. Host reading and macroexpansion would bypass the source, namespace, and staging contracts.
- Let embedding callers construct or execute private IR. This would publish compiler internals and weaken the single seam.

## Consequences

- A semantic fix in the compiler benefits AOT, REPL, ASDF, and embedding together, increasing leverage and locality.
- Interactive evaluation pays compilation latency but executes native code afterward.
- Adapters stay shallow and can be tested against one compiler/runtime contract.
- Compiler-session and staging behavior must support both multi-form modules and small interactive units.
- Native artifacts remain usable without retaining the compiler session that produced them.

## References

- [Compiler session](../domain.md#compiler-session)
- [Compilation unit](../domain.md#compilation-unit)
- [Native artifact](../domain.md#native-artifact)
- [Runtime ABI](../domain.md#runtime-abi)
- [Decision 0001: Native compilation replaces runtime tree walking](0001-native-compilation-over-runtime-evaluation.md)
- [Decision 0002: One deep compiler seam owns a private semantic IR](0002-deep-compiler-seam-private-semantic-ir.md)
