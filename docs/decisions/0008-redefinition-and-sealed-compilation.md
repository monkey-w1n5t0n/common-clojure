# Development linkage preserves redefinition

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

Clojure development relies on redefining Var roots and on dynamic Vars, while native performance benefits from resolved calls, specialized arities, inlining, and devirtualization. Letting the optimizer silently assume definitions are final would make reload behavior depend on incidental compiler choices. Routing every call through unrestricted runtime lookup would preserve flexibility at a permanent cost.

## Decision

Development compilation preserves Var redefinition by default. Calls to resolved, non-dynamic function Vars may be native direct calls, but they use stable linkage whose target can be replaced when the Var root changes. Dynamic Vars and genuinely higher-order targets use dynamic invocation. Optimizations that would make later redefinition invisible are not applied implicitly.

Sealed compilation is a separate, explicit semantic contract. It declares selected definitions final for the produced artifact and authorizes stronger inlining, arity specialization, and dispatch devirtualization. Redefining sealed definitions may be rejected or is outside that artifact's guarantee. Exact declaration syntax and granularity remain open implementation choices.

## Alternatives considered

- Route every global call through Var lookup and `funcall`. This is simple and fully dynamic but gives up avoidable native-call performance.
- Treat all compiled definitions as final. This maximizes optimization but breaks the interactive development model without warning.
- Let SBCL inline whenever it judges profitable. This makes Clojure redefinition semantics depend on opaque host optimization decisions.
- Maintain separate language semantics for development and release builds. This would make build mode change ordinary behavior rather than only behavior explicitly sealed by the program.

## Consequences

- Native linkage machinery must update coherently with Var roots and known arities.
- Ordinary direct call does not mean immutable or inlined call.
- Development artifacts retain predictable reload behavior with some indirection or inhibited inlining.
- Sealed artifacts can approach closed-world Common Lisp performance, but sealing is visible, reviewable, and testable.
- Artifact metadata must record assumptions needed to diagnose incompatible loading or redefinition.

## References

- [Var](../domain.md#var)
- [Direct call](../domain.md#direct-call)
- [Dynamic invocation](../domain.md#dynamic-invocation)
- [Stable linkage](../domain.md#stable-linkage)
- [Sealed compilation](../domain.md#sealed-compilation)
- [Decision 0005: Namespaces and Vars are semantic objects](0005-semantic-namespaces-and-vars.md)
