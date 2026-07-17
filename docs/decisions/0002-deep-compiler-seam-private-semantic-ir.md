# One deep compiler seam owns a private semantic IR

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

Clojure semantics differ from raw Common Lisp in truthiness, empty-list behavior, name resolution, Vars, macro staging, multi-arity functions, and validated tail recursion. Directly translating each source form to a host form spreads those decisions across reader macros and emitters. Conversely, exposing an extensible compiler tree would make every library extension depend on unstable compiler knowledge.

## Decision

The compiler is one deep module. Its small external seam supports creating an isolated compiler session, compiling a file or source module to a native artifact, and compiling source text as a native interactive thunk. Exact operation and package names remain implementation choices.

Behind that seam, macroexpansion and semantic analysis produce a closed, private semantic IR. The IR records resolved meaning needed for validation, diagnostics, representation selection, and lowering. It is not public, serializable, or extensible by arbitrary node registration. Lowering consumes it and emits forms for the sole SBCL target.

Behavioral tests and callers use the external compiler seam. Internal compiler tests may use internal seams without promoting them into public contracts.

## Alternatives considered

- Translate reader forms directly to Common Lisp. This is initially smaller but loses locality for semantic validation, source diagnostics, and optimization.
- Publish the IR as a compiler-plugin interface. This maximizes theoretical flexibility but freezes internals and creates a shallow module whose callers must understand its implementation.
- Expose every pipeline phase as a public module. This permits replacement of unproven stages at the cost of ordering constraints and duplicated compiler knowledge in callers.
- Introduce a generic backend interface. With only SBCL approved, one adapter would create a hypothetical seam rather than useful leverage.

## Consequences

- The IR earns its cost by centralizing resolved call kinds, Var access, tail targets, evaluation order, source provenance, and representation facts.
- Compiler changes remain local and do not force library ports to track private nodes.
- Debug tooling that needs semantic detail must be provided intentionally rather than by exposing raw IR.
- The compiler seam can remain stable while the IR and lowering strategy change substantially.
- A new public extension or backend seam requires concrete repeated use cases and a separate decision.

## References

- [Compiler session](../domain.md#compiler-session)
- [Semantic analysis](../domain.md#semantic-analysis)
- [Private semantic IR](../domain.md#private-semantic-ir)
- [Lowering](../domain.md#lowering)
- [Decision 0003: SBCL is the sole backend and a direct dependency](0003-sbcl-only-backend.md)
- [Decision 0006: Extend through language and runtime seams](0006-language-and-runtime-extension-seams.md)
