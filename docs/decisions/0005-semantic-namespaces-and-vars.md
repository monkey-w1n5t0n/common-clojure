# Namespaces and Vars are semantic objects, not CL packages

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

Clojure namespaces carry aliases, refers, mappings, and Var identities. Vars carry metadata, replaceable roots, and optional thread-local dynamic bindings. Common Lisp packages and its separate value and function namespaces provide useful implementation machinery but do not have the same semantics.

## Decision

Common Clojure maintains its own semantic namespace registry and stable Var objects. A Var's identity is separate from its current root. Dynamic Vars support thread-local dynamic bindings that shadow their roots.

Generated Common Lisp packages and symbols are private code-generation adapters. They provide deterministic native names and linkage but do not define Clojure resolution, visibility, aliases, or namespace identity.

The compiler distinguishes value access, Var-object access, direct calls, and dynamic invocation during semantic analysis. Resolved non-dynamic calls may use stable native linkage; value-position and dynamic operations preserve Var semantics.

## Alternatives considered

- Map each Clojure namespace directly to a CL package and each Var to a CL symbol. Package use/import rules, case behavior, and dual namespaces would leak into Clojure semantics.
- Compile every definition as an immutable lexical/global binding. This loses Var identity, redefinition, metadata, and dynamic binding.
- Resolve every access through a namespace hash table at runtime. This preserves dynamism but gives up compile-time diagnostics and avoidable native-call overhead.
- Treat function and value references as unrelated host names. This violates Clojure's unified Var model.

## Consequences

- The analyzer, runtime registry, and generated linkage must remain coordinated.
- Namespace aliases and refers are compile-time semantic facts, not CL package operations.
- CL code can embed Common Clojure through an intentional Var/invocation interface rather than package internals.
- Stable linkage can preserve redefinition while still allowing native direct calls.
- Symbol mangling and generated package layout remain private and may change without changing Clojure namespace identity.

## References

- [Namespace](../domain.md#namespace)
- [Var](../domain.md#var)
- [Root](../domain.md#root)
- [Dynamic binding](../domain.md#dynamic-binding)
- [Stable linkage](../domain.md#stable-linkage)
- [Decision 0008: Development linkage preserves redefinition](0008-redefinition-and-sealed-compilation.md)
