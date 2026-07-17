---
stability: aspirational
layer: behavioural
audience: both
---

# Value Model

> Spec: observable values, truthiness, equality, hashing, and numeric identity in Common Clojure. Counterpart to [MAIN.md](MAIN.md).
> See also [collections-and-sequences.md](collections-and-sequences.md) and [common-lisp-interop.md](common-lisp-interop.md).

## Source files

- `cl-clojure-syntax.lisp` — legacy evidence of value conflation that conforming code must replace.
- `cl-clojure-eval.lisp` — legacy truthiness and equality experiments; not conforming architecture.
- `clojure-tests/data_structures.clj` — host-independent value examples.
- `clojure-tests/numbers.clj` — numeric reference evidence where non-JVM-specific.

---

## 1. Frame

1.1 Common Clojure values preserve Clojure-observable identity and equality while using native SBCL representations where those representations do not collapse distinctions.

**Why:** Host efficiency is valuable only when programs still observe Clojure semantics.

1.2 Representation is private except for the explicit host mappings stated in this spec and [common-lisp-interop.md §2.1](common-lisp-interop.md).

---

## 2. Nil, booleans, and empty list

2.1 Clojure nil maps to Common Lisp `NIL`; Clojure true maps to `T`; Clojure false is a distinct singleton that is neither `NIL` nor `T`; and the empty Clojure list is a fourth, distinct singleton.

**Why:** Clojure observes nil, false, and empty list as unequal values, while both nil and false are falsey and the empty list is truthy.

2.2 Only nil and false are falsey. Every other value, including zero, the empty string, and the empty list, is truthy.

2.3 Predicates and boolean operators return canonical true or canonical false, not arbitrary Common Lisp generalized booleans.

2.4 `seq` of an empty collection returns nil; `rest` returns an empty sequence value; `next` returns nil when no next element exists.

**Why:** These distinctions are common control-flow inputs and must not depend on representation accidents.

2.5 Printing and reading MUST distinguish `nil`, `false`, `true`, and `()` and round-trip each to the same value class.

---

## 3. Equality and hashing

3.1 Clojure equality is value equality, not Common Lisp `EQ`, `EQL`, or `EQUAL` exposed directly.

3.2 Nil, false, and empty list are pairwise unequal. Equal sequential collections compare element-by-element even when their concrete sequential collection types differ.

3.3 Maps are equal when they contain equal key/value associations regardless of iteration order; sets are equal when they contain equal members regardless of iteration order.

3.4 Numeric equality follows Clojure numeric value semantics across supported numeric representations; identity-sensitive predicates remain separate.

3.5 Values that are equal under Clojure equality MUST produce the same Clojure hash. Metadata MUST NOT participate in equality or hashing.

**Why:** Persistent maps, sets, caches, and multimethod dispatch all depend on equality/hash coherence.

3.6 Host objects without a declared Clojure equality bridge compare by stable host identity unless their adapter explicitly defines value equality and matching hash behavior.

---

## 4. Numeric values

4.1 Native SBCL fixnums, bignums, ratios, and floating-point values MAY back corresponding Common Clojure numbers when their observable arithmetic agrees with the supported Clojure operation.

4.2 Integer arithmetic MUST promote rather than silently wrap when a result exceeds fixnum range, except inside an explicitly named unchecked operation.

**Why:** Numeric value preservation is the default Clojure contract; unchecked arithmetic must be visible at the call site.

4.3 Ratio arithmetic MUST remain exact until an operation explicitly requests floating-point conversion.

4.4 Floating-point NaN, infinity, signed zero, comparison, and hash behavior MUST be specified by focused tests before being claimed compatible; JVM bit-level representations are not implied.

---

## 5. Value conversion

5.1 Conversion between host and Clojure values occurs only through declared bridges. Raw host `NIL` maps to Clojure nil unless the bridge declares a generalized-boolean result, in which case it maps to canonical false.

**Why:** Common Lisp uses the same object for an empty list, false, and absent value, while Common Clojure cannot.

5.2 A conversion MUST either produce a value satisfying this model or signal the typed interop failure defined by [common-lisp-interop.md §5.1](common-lisp-interop.md).

---

## Open / Deferred

- **Which floating-point edge cases intentionally differ from JVM Clojure?** Resolve with an explicit numeric conformance matrix.
- **Should additional host numeric classes participate directly in Clojure numeric equality?** Require a real interop or library consumer before expanding the bridge.
