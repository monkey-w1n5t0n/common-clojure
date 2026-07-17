---
stability: aspirational
layer: behavioural
audience: both
---

# Namespaces, Vars, and Macros

> Spec: semantic namespaces, stable Vars, dynamic binding, resolution, and sequential native macro expansion. Counterpart to [MAIN.md](MAIN.md).
> See also [compiler-and-artifacts.md](compiler-and-artifacts.md), [reader-and-forms.md](reader-and-forms.md), and [common-lisp-interop.md](common-lisp-interop.md).

## Source files

- `cl-clojure-eval.lisp` — legacy Var and namespace experiments; semantic evidence only.
- `package.lisp` — current package exports; not the conforming namespace design.
- `clojure-tests/ns_libs.clj` — host-independent namespace examples.
- `clojure-tests/vars.clj` — host-independent Var examples.
- `clojure-tests/macros.clj` — macro behavior evidence.

---

## 1. Frame

1.1 Common Clojure namespaces and Vars have Clojure semantics independent of Common Lisp package and symbol-cell behavior.

**Why:** CL packages are useful backing infrastructure but do not implement aliases, refers, Var identity, metadata, or dynamic bindings by themselves.

1.2 CL packages and symbols MAY back resolved definitions after analysis, but this mapping is an adapter hidden from source programs.

---

## 2. Namespaces and resolution

2.1 A namespace is a stable named registry of mappings, aliases, refers, metadata, and loaded-module identity.

2.2 Source symbol resolution checks lexical bindings before namespace mappings. Qualified symbols resolve through their explicit namespace or alias; unqualified symbols use the current namespace and its refers according to Clojure conflict rules.

2.3 An unresolved or ambiguous symbol MUST signal an `analysis-error` at its source span; the compiler MUST NOT guess a host package symbol or fall back to a global user namespace.

**Why:** Silent fallback made the legacy environment appear compatible while binding the wrong definitions.

2.4 Namespace declarations and dependency clauses take effect in source order within the module staging world. Cyclic dependencies MUST report a typed dependency failure with the cycle path.

2.5 Auto-resolved keywords use the active semantic namespace and aliases at read/analysis time as appropriate; they MUST NOT be hard-coded to `user`.

---

## 3. Vars and redefinition

3.1 A Var has stable identity, namespace-qualified name, root state, metadata, and a dynamic flag. Re-evaluating `def` updates the existing Var rather than replacing its identity.

**Why:** Var references, metadata, watches, and redefinition depend on stable identity.

3.2 Reading a Var as a value returns its current binding or root according to dynamic-binding rules; Var quote returns the Var object itself.

3.3 Calls to ordinary non-dynamic function Vars use a redefinable native fdefinition kept coherent with the Var root, so existing compiled callers observe later redefinition without interpreter dispatch.

**Why:** This preserves interactive redefinition while retaining ordinary SBCL call performance.

3.4 A Var explicitly marked sealed/final MAY be direct-linked or inlined. Attempting to redefine it MUST signal unless the program explicitly removes that assertion and recompiles dependent callers.

**Why:** Direct linkage is an opt-in semantic promise, not a silent compiler mode that breaks redefinition.

3.5 Setting a function Var root, `alter-var-root`, and temporary root redefinition MUST keep the callable fdefinition and Var view coherent, including restoration during non-local exit.

---

## 4. Dynamic Vars

4.1 A dynamic Var maps to an SBCL special binding while retaining its Var identity and root.

4.2 Dynamic binding is thread-local, nestable, visible to native callees, and restored on normal return or non-local exit.

4.3 Binding a Var not declared dynamic MUST signal an analysis or runtime error at the earliest reliable phase.

4.4 A call through a dynamic function Var MUST observe the current dynamic binding rather than a static fdefinition shortcut.

---

## 5. Macros

5.1 A macro is a Var marked as a macro whose root is an SBCL-compiled function from source forms and lexical expansion context to a replacement form.

5.2 A top-level macro definition is compiled and installed in the module's staging world before the next top-level form expands. The compiled artifact also contains the definition needed by dependent module compilation.

**Why:** Sequential native staging permits ordinary Clojure macro-defined language growth without an evaluator.

5.3 Macro expansion repeats until the head is no longer a macro call, while preserving source origin and an expansion trace linking generated forms to call sites.

5.4 Macro functions receive Clojure-equivalent `&form` and `&env` information. The environment describes lexical names and supported compiler facts but does not expose the private IR.

5.5 Macro expansion MAY execute arbitrary compile-time Common Clojure code available in the compilation session. Failures are reported as specified by [diagnostics.md §2.2](diagnostics.md).

5.6 Syntax quote resolves namespace-qualified symbols, preserves metadata, creates hygienic auto-gensyms, and honors unquote and unquote-splicing in their valid contexts.

5.7 Macro expansion output is analyzed exactly like written source; a macro MUST NOT inject unchecked raw runtime code around the analyzer.

**Why:** One analyzer preserves name, control-flow, type, and failure semantics for generated code.

---

## 6. Visibility and metadata

6.1 Public/private, dynamic, macro, deprecation, type-hint, and final/sealed attributes are represented as Var or definition metadata with Clojure-observable lookup behavior where applicable.

6.2 Namespace and Var metadata follow [protocols-types-and-metadata.md §5.1](protocols-types-and-metadata.md) and do not alter Var identity.

---

## Open / Deferred

- **Which namespace dependency syntax should be retained unchanged from Clojure?** The semantic roles are fixed; aliases for CL systems and adapted libraries remain to be designed.
- **Should final/sealed status be reversible within a live session?** Reversibility must define dependent-code invalidation before becoming a promise.
- **How much compiler information should `&env` expose?** Add only facts needed by a real macro adaptation, without exposing private representation.
