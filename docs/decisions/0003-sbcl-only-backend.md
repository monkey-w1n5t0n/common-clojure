# SBCL is the sole backend and a direct dependency

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

The desired outcome is a language that uses SBCL's native compiler and infrastructure, not a portable compiler framework. Designing for hypothetical Common Lisp implementations or unrelated targets would constrain representation and lowering before a second target has supplied any real requirements.

## Decision

SBCL is the only compiler backend and an explicit platform dependency. Lowering may emit Common Lisp and SBCL-specific declarations or facilities when they improve semantics, diagnostics, linkage, concurrency, or performance.

The SBCL emitter remains internally local to the compiler, but there is no public backend abstraction or portability guarantee. FASLs are treated as SBCL artifacts tied to a compatible build environment.

## Alternatives considered

- Restrict output to portable ANSI Common Lisp. This would sacrifice approved SBCL leverage for a portability outcome the project does not require.
- Define a backend interface now for other Common Lisp implementations. No second adapter exists, so the interface would encode guesses and expose compiler internals prematurely.
- Target the JVM as an additional backend. This contradicts the hosted-language goal and would reintroduce the platform whose infrastructure the project is intentionally replacing.
- Emit C or LLVM directly. This bypasses rather than uses the Common Lisp and SBCL infrastructure at the heart of the project.

## Consequences

- The compiler may use SBCL source notes, type declarations, native threading, atomic operations, and linkage behavior directly.
- Artifacts are not promised to load on other Common Lisp implementations or incompatible SBCL environments.
- Some runtime and host-bridge code will be platform-specific.
- If a second backend becomes an approved product goal with a working adapter, its actual differences can justify a new seam and superseding decision.

## References

- [Language and compatibility specification](../specs/language-and-compatibility.md)
- [Hosted language](../domain.md#hosted-language)
- [FASL](../domain.md#fasl)
- [Platform adaptation](../domain.md#platform-adaptation)
- [Decision 0002: One deep compiler seam owns a private semantic IR](0002-deep-compiler-seam-private-semantic-ir.md)
