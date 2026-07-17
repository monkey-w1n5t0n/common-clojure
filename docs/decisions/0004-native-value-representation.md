# Native value representation preserves Clojure distinctions

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

Common Lisp and Clojure overlap in many scalar representations but disagree at important semantic edges. Common Lisp uses `NIL` for false and the empty list, whereas Clojure distinguishes `nil`, `false`, and the truthy empty list. Raw mutable CL vectors and hash tables also do not provide Clojure persistent-value, equality, or hashing semantics.

## Decision

The runtime represents Clojure `nil` as Common Lisp `NIL` and Clojure `true` as Common Lisp `T`. Clojure `false` is a distinct interned singleton. The empty Clojure list is a distinct immutable, truthy singleton rather than Common Lisp `NIL`.

Native host scalars are reused where their observable semantics align, including suitable numbers, characters, and strings. Symbols, keywords, metadata-bearing values, and persistent collections are runtime-owned Clojure representations. Clojure equality and hashing are centralized runtime semantics and are not delegated indiscriminately to a Common Lisp equality predicate or hash-table test.

The compiler may use unboxed host predicates internally when a value does not escape, but crossing the runtime ABI must restore the canonical Clojure value.

## Alternatives considered

- Represent `false` as `NIL`. This collapses `false` and `nil`, violating equality, predicates, printing, and collection behavior.
- Represent Clojure `nil` as a new object and use `NIL` for false. This complicates host nil interoperability and still requires explicit Clojure truthiness.
- Represent the empty Clojure list as `NIL`. This makes it falsey and indistinguishable from `nil`.
- Use raw CL lists, vectors, and hash tables as Clojure collections. Their mutability, equality, hashing, metadata, and empty-value behavior do not satisfy the semantic contract.
- Wrap every host value. This preserves control but discards native scalar performance and interoperability where no semantic mismatch exists.

## Consequences

- Conditional lowering must implement Clojure truthiness rather than emit an unchecked CL condition.
- False predicate results may require boxing when they become Clojure values; branch-only results can remain optimized internally.
- Host collection adapters must not leak mutation into persistent values.
- Persistent implementation strategies may change behind the runtime ABI while preserving value, equality, and hashing behavior.
- Changing these canonical identities would require an intentional runtime-ABI transition and recompilation of dependent artifacts.

## References

- [Value-model specification](../specs/value-model.md)
- [Runtime ABI](../domain.md#runtime-abi)
- [Persistent value](../domain.md#persistent-value)
- [Semantic compatibility](../domain.md#semantic-compatibility)
