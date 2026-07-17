---
stability: aspirational
layer: behavioural
audience: both
---

# Collections and Sequences

> Spec: persistent collection behavior, sequence abstraction, laziness, transients, and collection invocation. Counterpart to [MAIN.md](MAIN.md).
> See also [value-model.md](value-model.md), [protocols-types-and-metadata.md](protocols-types-and-metadata.md), and [performance.md](performance.md).

## Source files

- `cl-clojure-eval.lisp` — legacy collection and lazy-range behavior; semantic evidence only.
- `cl-clojure-transducers.lisp` — legacy transducer algorithms with non-conforming eager cutoffs.
- `clojure-tests/vectors.clj` — vector reference evidence.
- `clojure-tests/data_structures.clj` — map and collection reference evidence.
- `clojure-tests/clojure_set.clj` — set reference evidence.
- `clojure-tests/sequences.clj` — sequence reference evidence.

---

## 1. Frame

1.1 Common Clojure provides immutable persistent lists, vectors, maps, and sets plus a common sequence abstraction with Clojure-observable behavior.

**Why:** Persistent data and sequence operations are foundational to the language and every named target library.

1.2 Collection representation is private. Native arrays, hash tables, conses, and trees MAY be used internally only when updates, equality, hashing, metadata, ordering, and persistence remain conforming.

---

## 2. Persistent collections

2.1 An operation on a persistent collection MUST NOT change the value observed through any existing reference to that collection.

2.2 Vector append, indexed association, map association/removal, and set addition/removal return values with structural sharing suitable for repeated functional updates; they MUST NOT copy the complete collection for every ordinary update at scale.

**Why:** Persistence without structural sharing satisfies immutability superficially but fails the native-performance mission and target-library workloads.

2.3 Vectors preserve index order and support integer lookup. Maps preserve key/value associations independent of iteration order. Sets preserve membership independent of iteration order. Lists preserve sequence order and front-oriented construction.

2.4 Maps support nil, false, and the empty list as distinct keys. Presence tests MUST distinguish an absent key from a present key mapped to nil.

2.5 Empty list is the truthy singleton defined by [value-model.md §2.1](value-model.md); it is neither nil nor a host `NIL` list terminator exposed as a Clojure list.

2.6 Collection equality and hashes follow [value-model.md §3.1](value-model.md), including sequential cross-type equality and order-independent map/set hashing.

---

## 3. Sequence behavior and laziness

3.1 `seq` returns nil for an empty seqable value and a sequence view for a non-empty value. Sequence views do not require copying the whole source collection.

3.2 `first`, `rest`, and `next` obey [value-model.md §2.4](value-model.md) for empty and terminal sequences.

3.3 Lazy sequence operations MUST be demand-driven and MUST NOT impose arbitrary realization limits on finite or infinite inputs.

**Why:** The legacy implementation's fixed 1,000/10,000-element limits changed program results and hid nontermination rather than representing laziness.

3.4 Realizing a lazy element caches its successful result so repeated traversal does not repeat the element's producer side effects. Concurrent realization MUST not publish a partially initialized value.

3.5 Operations documented as lazy return before consuming an unbounded input and consume no more input than required to produce the demanded prefix.

3.6 Sequence functions accept every declared seqable type, including strings, maps as map entries, sets, records where applicable, and explicit CL bridge values. Unsupported values signal a typed not-seqable condition.

3.7 Reduced values terminate reductions without consuming later input. Transducers compose independently of a source collection and preserve completion arities and reduced propagation.

---

## 4. Collection invocation

4.1 Keywords invoked with a map-like target return the associated value or nil, and accept an optional not-found value.

4.2 Maps invoked with a key perform map lookup and accept an optional not-found value.

4.3 Sets invoked with a value return the stored equal member or nil; `contains?` remains the presence operation when the member itself may be nil.

4.4 Vectors invoked with an integer return the indexed value and follow the vector lookup failure/default behavior.

4.5 Wrong invocation arity or an invalid index/key domain signals the same typed failure whether the operation is reached through invocation or its named lookup function.

---

## 5. Transients and builders

5.1 A transient is a linear, owner-scoped optimization for building a persistent collection; converting it to persistent invalidates further transient mutation.

5.2 Transient operations MUST NOT mutate persistent values that existed before transient creation.

5.3 Cross-owner use, use after persistence, or an operation unsupported by that transient type signals a typed transient-state condition.

5.4 Internal builders MAY use mutable native storage without exposing it, provided the published result obeys persistent semantics.

---

## 6. Failure behavior

6.1 Out-of-range indexed access signals when no not-found argument is defined and returns the explicit not-found value when that arity defines one.

6.2 Map and set construction reject malformed entry/member input rather than dropping data or silently choosing a duplicate whose equality is ambiguous.

6.3 A lazy producer condition is raised at the demand that encounters it, with both producer and consumer source context when available.

---

## Open / Deferred

- **Which sequence operations guarantee chunking, and is chunking observable enough to specify?** Decide from side-effect-sensitive compatibility cases and performance evidence.
- **What concurrent realization guarantee should apply after a lazy producer throws?** Successful caching is fixed; retry-versus-cache-failure semantics need focused tests.
- **Which specialized collection variants are part of the stable surface?** Add only when a target application demonstrates need.
