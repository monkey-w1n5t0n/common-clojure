# Semantic conformance and adapted libraries are the North Star

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

The official Clojure test corpus mixes language semantics with JVM classes, Java interop, bytecode generation, reflection, serialization, and implementation-specific details. Treating the entire suite as the goal drove placeholder Java behavior and rewarded files that merely loaded rather than trustworthy native semantics. The project instead exists to support new applications on SBCL with the Clojure behaviors that make those applications and selected libraries valuable.

## Decision

Progress is judged by explicit Common Clojure semantic conformance and by useful native applications and adapted libraries. The conformance corpus selects observable Clojure behaviors independent of JVM machinery and states intentional platform adaptations.

The official Clojure suite remains evidence and a source of cases, not the authoritative completion metric. Milestones include credible adapted ports of libraries such as spec, pattern matching, asynchronous channels, and rule systems, without promising those upstream libraries or arbitrary Clojure projects run unchanged.

Java/JVM behavior is included only when a Common Clojure semantic contract deliberately adopts or adapts it. Placeholder emulation does not count as conformance.

## Alternatives considered

- Make every official Clojure test pass. This conflates Clojure semantics with the JVM implementation and incentivizes broad stubbing.
- Report parse/load counts as progress. Those counts do not demonstrate assertion behavior or native execution.
- Use only bespoke application examples. This risks accidental semantic drift and gives no reusable compatibility contract.
- Adopt Common Lisp behavior whenever it is convenient. This would create a syntax dialect rather than Clojure-semantics hosted language.

## Consequences

- The project must curate and maintain a truthful semantic conformance corpus.
- JVM-specific tests can be excluded explicitly without being faked or treated as hidden failures.
- Semantic compatibility claims must name their covered behavior or adapted library; there is no blanket project compatibility claim.
- Library adaptations become architectural feedback for collections, macros, protocols, concurrency, and host bridges.
- Test counts from the legacy runner are not a project status metric.

## References

- [Language and compatibility specification](../specs/language-and-compatibility.md)
- [Semantic compatibility](../domain.md#semantic-compatibility)
- [Platform adaptation](../domain.md#platform-adaptation)
- [Adapted library](../domain.md#adapted-library)
- [Common Clojure](../domain.md#common-clojure)
