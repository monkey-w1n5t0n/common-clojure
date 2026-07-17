---
stability: aspirational
layer: cross-cutting
audience: both
---

# Protocols, Types, Records, Multimethods, and Metadata

> Spec: open protocol dispatch, native types and records, reification, arbitrary multimethod dispatch, and metadata. Counterpart to [MAIN.md](MAIN.md).
> See also [common-lisp-interop.md](common-lisp-interop.md), [value-model.md](value-model.md), and [namespaces-vars-and-macros.md](namespaces-vars-and-macros.md).

## Source files

- `cl-clojure-eval.lisp` — legacy protocol, record, multimethod, and metadata stubs; evidence only.
- `clojure-tests/protocols.clj` — protocol and record reference behavior.
- `clojure-tests/multimethods.clj` — multimethod reference behavior.
- `clojure-tests/metadata.clj` — metadata reference behavior.

---

## 1. Frame

1.1 Common Clojure protocols provide open, first-argument type dispatch over Common Clojure and explicitly bridged Common Lisp types.

**Why:** Protocols are the ordinary polymorphism seam for target libraries and host integration.

1.2 Protocol methods use native CLOS generic functions and methods as their SBCL dispatch substrate while preserving the Clojure-visible protocol descriptor and Var behavior.

**Why:** CLOS supplies native open extension and optimized dispatch without a parallel interpreted protocol table.

1.3 Multimethods remain distinct from protocols because they dispatch on arbitrary runtime values, hierarchies, and preferences rather than only host type.

---

## 2. Protocols

2.1 Defining a protocol creates a stable descriptor and callable method Vars with declared names, argument lists, documentation, and metadata.

2.2 Extending a type installs native methods for the declared protocol operations. Extension of nil is supported explicitly and does not treat false or empty list as nil.

2.3 Protocol dispatch selects on the first declared target argument. Other arguments participate in method execution but not protocol method selection.

2.4 Missing protocol implementation signals a typed condition naming the protocol, method, and runtime target type; it MUST NOT return nil or invoke a generic fallback accidentally.

2.5 Redefining or extending a protocol invalidates affected dispatch assumptions and becomes visible to subsequent calls, subject only to an explicit sealed/final assertion.

2.6 `satisfies?` reports whether a target has a conforming implementation, including extensions installed for bridged CL classes and nil.

---

## 3. Types, records, and reification

3.1 User-defined types compile to native SBCL classes or structures with compiled constructors, field access, and protocol methods; instances do not carry source-form method bodies.

3.2 Records additionally implement persistent associative lookup over declared fields and extension entries, preserve record type identity, and follow Clojure record equality/hash behavior.

3.3 Record and type field access MUST respect immutability promises. Explicit mutable fields, if supported, are named in the type declaration and use controlled mutation semantics.

3.4 Reification creates an instance of a generated native class implementing the requested protocols/interfaces and lexically capturing referenced values.

3.5 Generated classes and methods retain source identity sufficient for diagnostics but their host names are not a stable user-facing interface.

---

## 4. Multimethods and hierarchy

4.1 A multimethod contains a dispatch function, method table, default dispatch value, hierarchy, preference relation, and versioned cache.

4.2 Dispatch evaluates the dispatch function exactly once, selects the most specific applicable method under `isa?` and preferences, and invokes a native compiled method function.

4.3 Adding, replacing, removing, preferring, or changing hierarchy relationships invalidates every cached result whose answer may change.

**Why:** Stale dispatch caches would make live redefinition and hierarchy updates nondeterministic.

4.4 No applicable method signals a typed no-method condition unless a default method exists. Ambiguous incomparable methods signal an ambiguity condition naming the candidates.

4.5 Multimethod arbitrary-value dispatch MUST NOT be approximated as CLOS class dispatch.

---

## 5. Metadata

5.1 Metadata is an immutable map associated with metadata-capable values including symbols, collections, Vars, namespaces, types, and functions where declared.

5.2 `with-meta` returns a value with replacement metadata while sharing the underlying immutable payload; `vary-meta` applies its function once to the current map.

5.3 Metadata does not affect value equality or hashing, as owned by [value-model.md §3.5](value-model.md).

5.4 Source spans, compiler type hints, macro/final/dynamic/private flags, and user metadata remain distinguishable even when represented in one metadata map.

5.5 Type hints and other compiler metadata MAY change generated declarations and call specialization but MUST NOT change the program's value result or suppress a required runtime check unless the hint is an explicit unsafe assertion.

5.6 Asking for metadata from a value that does not support it returns nil; attempting to attach metadata to such a value signals a typed metadata condition.

---

## 6. Failure behavior

6.1 Invalid protocol signatures, duplicate methods, illegal field declarations, and unresolved extension targets are analysis failures at the declaration span.

6.2 A CLOS method-combination or class-redefinition limitation that prevents promised protocol/type behavior MUST be surfaced as a typed native-compilation or load failure, not hidden behind a slower interpreter path.

---

## Open / Deferred

- **Which CLOS class-redefinition cases are supported in a live session?** Preserve ordinary protocol extension first; promise instance migration only after SBCL behavior is characterized.
- **Should mutable deftype fields be in the initial stable core?** A real systems-programming use case should define the required memory and concurrency semantics.
- **Which metadata-capable host values receive wrappers versus native side storage?** The answer must preserve identity and reclamation without exposing representation.
