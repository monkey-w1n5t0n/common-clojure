# Extend through language and runtime seams

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

Common Clojure must support native applications and adapted libraries such as spec, pattern matching, asynchronous channels, and rule systems. Those libraries need expressive extension, but exposing compiler internals would couple them to representation and lowering choices before their actual needs are known.

## Decision

Supported extensions use ordinary Clojure-facing seams:

- macros that transform source forms into source forms;
- tagged literal readers;
- protocols, types, and multimethods;
- declarative host bridges for selected Common Lisp functions, values, and types;
- small Common Lisp adapter modules for platform facilities that cannot be expressed declaratively.

Macro lexical context, when exposed, is an abstract source-level view rather than private semantic IR. Host bridges state their callable/runtime contract and do not receive arbitrary emitter callbacks.

There is no public IR-node registry, arbitrary compiler-plugin interface, or backend extension point. A new compiler hook requires at least two concrete consumers with the same unmet need and a separate decision defining a narrow seam.

## Alternatives considered

- Publish the semantic IR and let libraries install analysis or lowering callbacks. This would maximize immediate power but freeze private compiler knowledge and weaken locality.
- Provide a general raw-Common-Lisp escape in every form. This would bypass Clojure analysis, diagnostics, and host-access policy throughout application code.
- Forbid host-specific extensions. This would make SBCL infrastructure inaccessible and force reimplementation of useful platform facilities.
- Emulate Java APIs so upstream libraries appear unchanged. This preserves superficial names while adding a second platform model and unsupported compatibility machinery.

## Consequences

- Most libraries remain portable within Common Clojure because they extend syntax and runtime behavior rather than compiler internals.
- Platform-heavy libraries require explicit adaptation instead of pretending JVM assumptions hold.
- A core.async adaptation may initially use SBCL concurrency primitives through adapters rather than copying JVM analyzer integration.
- Some highly optimized facilities may require a small co-developed runtime primitive.
- Exact host-bridge syntax and package names remain implementation choices.

## References

- [Macroexpansion](../domain.md#macroexpansion)
- [Protocol](../domain.md#protocol)
- [Multimethod](../domain.md#multimethod)
- [Host bridge](../domain.md#host-bridge)
- [Adapted library](../domain.md#adapted-library)
- [Decision 0002: One deep compiler seam owns a private semantic IR](0002-deep-compiler-seam-private-semantic-ir.md)
