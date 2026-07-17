---
stability: aspirational
layer: cross-cutting
audience: both
---

# Common Lisp and SBCL Interop

> Spec: explicit bridges between Common Clojure and its Common Lisp/SBCL host. Counterpart to [MAIN.md](MAIN.md).
> See also [value-model.md](value-model.md), [protocols-types-and-metadata.md](protocols-types-and-metadata.md), and [diagnostics.md](diagnostics.md).

## Source files

- `package.lisp` — existing host package surface; evidence only.
- `cl-clojure-eval.lisp` — legacy Java-emulation and host-call experiments that this contract replaces.
- `cl-clojure.asd` — current ASDF integration point.

---

## 1. Frame

1.1 Common Lisp interop is an explicit, native host bridge rather than Java syntax emulation or an unrestricted bypass around Common Clojure analysis.

**Why:** The host is an asset, but implicit conversion and raw compiler escape paths would make Clojure semantics unpredictable.

1.2 Ordinary language extensions use macros, tagged literals, protocols over declared CL types, declarative host bindings, or small CL adapter modules; they do not require a public compiler plugin interface.

---

## 2. Host values and names

2.1 Clojure nil is host `NIL`, Clojure true is host `T`, false is a distinct singleton, and empty Clojure list is a distinct truthy singleton as specified by [value-model.md §2.1](value-model.md).

2.2 Clojure symbols and keywords are not CL symbols. A host bridge resolves a declared CL package/name at analysis time and records whether it denotes a function, variable, class, type, generic function, condition, or macro adapter.

2.3 Source programs MUST opt into host names through a declared namespace/alias/import mechanism. Unqualified unresolved symbols MUST NOT search arbitrary CL packages.

2.4 A bridged host object remains a first-class opaque value unless an adapter declares Clojure equality, hashing, sequencing, lookup, printing, or protocol behavior.

---

## 3. Calls and conversion

3.1 A declared host function or generic function call emits an ordinary native CL call when its target is statically known; otherwise it uses an explicit callable adapter.

3.2 Arguments and results follow the bridge's declared conversion contract. Conversion MUST NOT infer from a returned `NIL` whether the host meant false, empty list, or no value.

**Why:** Those meanings collapse in Common Lisp and are distinct in Common Clojure.

3.3 A result declared as a generalized boolean maps host non-nil to canonical true and host nil to canonical false. An undeclared raw `NIL` result maps to Clojure nil.

3.4 Numeric and character values MAY pass without allocation when their supported semantics coincide. Strings MAY share native storage only while Common Clojure immutability remains observable.

3.5 Multiple host values, optional arguments, keyword arguments, and rest arguments require an explicit bridge declaration or adapter; they MUST NOT be guessed from Clojure call shape.

3.6 Type declarations supplied by a bridge MAY enable unboxed arithmetic, direct slot access, and native call specialization, subject to the safety tiers in [performance.md §2.1](performance.md).

---

## 4. CLOS and SBCL integration

4.1 Declared CL classes can receive Common Clojure protocol extensions through native CLOS methods as specified by [protocols-types-and-metadata.md §2.2](protocols-types-and-metadata.md).

4.2 Common Clojure types and records MAY be passed to CL code as their native class instances; adapter authors must not rely on generated class/package names unless those names are explicitly exported by the bridge.

4.3 SBCL-specific facilities MAY be exposed through named bridge namespaces or adapter systems. Their source MUST identify the SBCL dependency and failure behavior.

4.4 Host macros are not called as runtime functions. A CL macro used by Common Clojure MUST be wrapped by a compile-time adapter that returns analyzable Common Lisp emission or ordinary Common Clojure forms under the compiler's control.

---

## 5. Conditions and resources

5.1 Argument conversion failure, unresolved host name, wrong host kind, foreign type mismatch, or unsupported multiple-value shape signals a typed `interop-error` with source span and host cause.

5.2 Declared CL condition classes can participate in Common Clojure catch matching. Undeclared host conditions propagate with their original cause and acquire Common Clojure source context at the bridge frame.

5.3 Resource adapters MUST specify ownership and cleanup. Cleanup participates in Common Clojure `try/finally` and non-local exit restoration.

5.4 Host warnings and style warnings remain distinguishable from fatal conditions; compiler policy MAY promote named warning classes but MUST report the policy in diagnostics.

---

## 6. Compatibility limits

6.1 Java constructor, field, method, reflection, proxy, and class-generation behavior are not emulated. Familiar punctuation MAY be repurposed only if its Common Lisp meaning is documented and unambiguous.

6.2 A CL adapter MAY replace a JVM-specific implementation inside an adapted library while preserving that library's Common Clojure-visible contract.

6.3 Raw host-form escape, if later admitted, MUST be explicit, source-located, limited to compile-time emission, and unable to install a runtime interpreted path.

---

## Open / Deferred

- **What concrete source syntax declares host functions, variables, classes, and result conventions?** The behavioral roles are fixed; syntax should be proven in the first real adapter.
- **Which CL multiple-value conventions deserve standard adapters?** Decide from actual APIs used by Common Clojure projects.
- **Should a raw host-form escape exist at all?** Small adapter systems may provide enough power with stronger analysis and diagnostics.
