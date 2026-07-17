# Replace the legacy evaluator rather than layering over it

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

The legacy evaluator contains reader work, semantic experiments, core-function implementations, tests, Java-oriented stubs, and a large tree-walking dispatch path. It is useful evidence, but using it beneath or beside the native compiler would retain two execution models and encourage compatibility exceptions to become permanent architecture.

## Decision

Migration follows replace-not-layer discipline. New semantic slices run only through the native compiler and runtime ABI. The compiler does not fall back to the legacy evaluator, and new runtime modules do not depend on evaluator environments, interpreted closures, or evaluator dispatch.

Legacy reader algorithms, tests, and runtime functions may be brought across only as individually reviewed material. Reused behavior receives native-path tests at the new seam. Java emulation stubs and evaluator-specific compatibility paths are not migrated merely because they exist.

The legacy implementation remains isolated as historical evidence until the native path supersedes the behavior being consulted; isolation is not support for hybrid execution.

## Alternatives considered

- Put a compiler facade over the evaluator and replace internals gradually. This would make the legacy environment and values part of the new runtime ABI.
- Retain interpreter fallback for unsupported forms. Programs would change execution model midstream and unsupported semantics could remain hidden indefinitely.
- Share closures, environments, and dispatch between compiler and evaluator. This would optimize reuse rather than the approved native architecture.
- Delete all legacy material immediately. This would discard useful reader cases, semantic discoveries, and regression evidence before replacements exist.

## Consequences

- Early native slices may support less surface area than the legacy evaluator, but every supported slice validates the intended architecture.
- Useful functions require extraction and conformance review rather than bulk movement.
- Tests that assert evaluator internals do not constrain the new compiler; observable semantic cases can be retained.
- There is no compatibility requirement for legacy evaluator representations or internal interfaces.
- Once no unique evidence remains, removal can be considered without adding a deprecation layer.

## References

- [Legacy evaluator](../domain.md#legacy-evaluator)
- [Native vertical slice](../domain.md#native-vertical-slice)
- [Runtime ABI](../domain.md#runtime-abi)
- [Decision 0001: Native compilation replaces runtime tree walking](0001-native-compilation-over-runtime-evaluation.md)
