# Domain language

## Common Clojure

**Definition:** The Clojure-semantics hosted language whose programs are compiled through Common Lisp into native SBCL artifacts.

**Not:** A JVM compatibility layer, a general implementation for unmodified Clojure projects, or Clojure syntax interpreted at runtime.

**Relationships:** Common Clojure is a **hosted language** with **semantic compatibility** as its compatibility target, a **host bridge** for deliberate Common Lisp access, and a **runtime ABI** supporting its native artifacts. Its architecture is fixed by the [accepted decisions](decisions/).

## Hosted language

**Definition:** A language whose semantics are defined independently but whose compilation, runtime, and platform facilities deliberately reuse a host language and implementation.

**Not:** A textual dialect that simply inherits every host behavior, or an emulation of another platform.

**Relationships:** Common Clojure is hosted by Common Lisp on SBCL; **platform adaptations** state where host facilities intentionally replace JVM-specific facilities.

## Compiler session

**Definition:** The explicit compilation context that owns configuration, dependency resolution, and the evolving compile-time world used across one or more compilation units.

**Not:** The runtime namespace registry, a global REPL environment, or a compiled artifact.

**Relationships:** A compiler session owns a **staging world**, performs **macroexpansion** and **semantic analysis**, and produces **native artifacts**. Separate sessions isolate concurrent or experimental compilation.

## Compilation unit

**Definition:** The ordered batch of source forms analyzed and committed as one compiler transaction and emitted as one coherent native artifact or native thunk.

**Not:** Necessarily a namespace, package, file, or whole application.

**Relationships:** A compilation unit may represent a **source module** or a REPL submission, is processed within a **compiler session**, and commits its staged definitions only after successful compilation.

## Source module

**Definition:** A dependency-addressable body of Common Clojure source forms that declares the namespace in which its definitions are analyzed.

**Not:** A Common Lisp package or an arbitrary grouping of runtime objects.

**Relationships:** A source module is commonly compiled as a **compilation unit**, may require other source modules, and contributes definitions to a semantic **namespace**.

## Source form

**Definition:** A Clojure data value in the source and expansion layer that retains enough origin information for macro processing and diagnostics.

**Not:** An analyzed expression, private IR node, or emitted Common Lisp form.

**Relationships:** Source forms carry **source spans**, are transformed by **macroexpansion**, and become semantic IR only through **semantic analysis**. Their observable contract is owned by the [reader and forms specification](specs/reader-and-forms.md).

## Source span

**Definition:** The source name and positional extent associated with a source form or derived semantic expression.

**Not:** Runtime metadata or a stack trace.

**Relationships:** Source spans survive macroexpansion and semantic analysis so diagnostics and native debug information can refer back to source.

## Macroexpansion

**Definition:** The compile-time execution of a macro Var that transforms a source form into another source form before semantic analysis.

**Not:** Runtime interpretation, arbitrary private-IR mutation, or host macro expansion performed without a Common Clojure macro contract.

**Relationships:** Macroexpansion reads definitions from the **staging world**, may receive an abstract lexical environment, and must preserve an expansion trace and source provenance.

## Staging world

**Definition:** The provisional namespace and Var state in which earlier forms of a compilation unit are visible to later macroexpansion and analysis without yet committing the unit globally.

**Not:** The loaded runtime world or an unrestricted copy of process-global state.

**Relationships:** A **compiler session** creates a staging world for a compilation unit; successful compilation commits it, while failed compilation discards it.

## Semantic analysis

**Definition:** The compile-time resolution and validation of expanded source forms into expressions with explicit lexical, Var, call, control-flow, representation, and effect meaning.

**Not:** Macroexpansion, Common Lisp emission, runtime evaluation, or general static typing.

**Relationships:** Semantic analysis consumes macroexpanded **source forms**, resolves **namespaces** and **Vars**, validates `recur` and call semantics, and produces the **private semantic IR**.

## Private semantic IR

**Definition:** The compiler-owned, closed representation of validated Common Clojure meaning between semantic analysis and lowering.

**Not:** A public extension interface, a serialized compatibility contract, a generic property bag, or a promised multi-backend representation.

**Relationships:** It records resolved operations such as local access, Var access, calls, control flow, definitions, and host operations; **lowering** consumes it. See [Decision 0002](decisions/0002-deep-compiler-seam-private-semantic-ir.md).

## Lowering

**Definition:** The semantics-preserving translation of private semantic IR into host forms and runtime operations suitable for native SBCL compilation.

**Not:** Macroexpansion, semantic analysis, or a public backend interface.

**Relationships:** Lowering chooses representations, **direct calls**, **dynamic invocation**, and **stable linkage**, then hands Common Lisp forms to SBCL.

## Native artifact

**Definition:** SBCL-compiled executable output whose Common Clojure functions run as native compiled functions against the runtime ABI.

**Not:** Source text, emitted-but-uncompiled Common Lisp, an interpreter cache, or a JVM artifact.

**Relationships:** A FASL is the normal ahead-of-time native artifact; a REPL submission may instead produce an in-memory native thunk through the same compiler path.

## FASL

**Definition:** A loadable SBCL artifact produced from the lowered Common Lisp representation of a compilation unit.

**Not:** A portable Common Lisp interchange format or a compatibility promise across SBCL versions and build environments.

**Relationships:** A FASL is one form of **native artifact** and links against the **runtime ABI**.

## Runtime ABI

**Definition:** The stable set of runtime representations, linkage conventions, and callable operations on which native artifacts depend.

**Not:** The compiler interface, private semantic IR, or every internal runtime function.

**Relationships:** The runtime ABI covers canonical values, persistent values, Var linkage, protocols, multimethods, conditions, and host-bridge calls required by loaded artifacts.

## Namespace

**Definition:** The semantic registry that maps Clojure names to Vars and records aliases, refers, and metadata.

**Not:** A Common Lisp package, source file, lexical scope, or generated code container.

**Relationships:** A **source module** declares a namespace; semantic analysis resolves qualified and unqualified names through it; generated Common Lisp packages are private adapters only.

## Var

**Definition:** The stable, namespace-qualified identity of a named Clojure definition, carrying metadata and a replaceable root binding.

**Not:** A lexical local, a Common Lisp symbol alone, or the current root value itself.

**Relationships:** A Var belongs to a **namespace**, has a **root**, may participate in **dynamic binding**, and provides **stable linkage** for compiled code.

## Root

**Definition:** The process-wide default value currently associated with a Var when no thread-local dynamic binding shadows it.

**Not:** The Var identity or a lexical binding.

**Relationships:** Redefinition replaces a Var's root without replacing the Var; dynamic lookup falls back to the root when no **dynamic binding** exists.

**Scenarios:** Defining a function installs its compiled function as the Var's root. Binding a dynamic Var within one thread does not replace that root.

## Dynamic binding

**Definition:** A thread-local, dynamically scoped value that temporarily shadows the root of a Var declared dynamic.

**Not:** Lexical shadowing, global redefinition, or an arbitrary binding of a non-dynamic Var.

**Relationships:** Dynamic invocation must consult a Var's active dynamic binding; host special-variable facilities may implement this behavior without becoming its semantic definition.

**Scenarios:** A request binds an output Var while executing a handler. A direct call to an ordinary non-dynamic function Var does not perform dynamic lookup.

## Direct call

**Definition:** A compiled invocation whose target Var and arity are resolved and whose native entry point can be selected without fetching an arbitrary runtime function value.

**Not:** Inlining, a promise that redefinition is ignored, or invocation of a lexical function value.

**Relationships:** Development direct calls use **stable linkage** to preserve redefinition; **sealed compilation** may authorize stronger optimization.

**Scenarios:** Calling a resolved non-dynamic `sum` Var at a known arity is direct. Calling a function passed as a parameter is dynamic invocation.

## Dynamic invocation

**Definition:** Invocation that obtains or dispatches through a runtime callable value because the target cannot be fixed as one direct native entry point.

**Not:** Dynamic Var binding specifically, though a dynamically bound Var may require dynamic invocation.

**Relationships:** Higher-order calls, dynamic Vars, multimethods, and some protocol calls use dynamic invocation or specialized runtime dispatch.

**Scenarios:** Applying a function argument uses dynamic invocation. A known protocol operation may instead lower to its protocol dispatch entry point.

## Stable linkage

**Definition:** The indirection identity retained by compiled call sites so replacing a Var root can redirect future calls without recompiling callers.

**Not:** General name lookup on every invocation or permission to inline a redefinable root.

**Relationships:** Stable linkage reconciles **direct calls** with development-time redefinition and is relaxed only by explicit **sealed compilation**.

## Sealed compilation

**Definition:** An explicit compilation contract declaring selected definitions final for an artifact so the compiler may optimize across linkage and dispatch points that development compilation must preserve.

**Not:** An optimizer guess, the default build mode, or a claim that all application state is immutable.

**Relationships:** Sealed compilation may inline or devirtualize eligible direct calls and rejects or does not guarantee later redefinition of those sealed definitions.

## Persistent value

**Definition:** A Clojure value whose apparent updates produce a new value while prior versions remain observably unchanged.

**Not:** Merely a mutable host collection hidden by convention or a value that survives process restart.

**Relationships:** Persistent lists, vectors, maps, and sets are runtime-owned representations governed by centralized Clojure equality and hashing; canonical value distinctions are owned by the [value-model specification](specs/value-model.md).

**Scenarios:** Associating a key returns a map while the original map retains its old contents. A transient builder may mutate internally but is not a persistent value until made persistent.

## Protocol

**Definition:** A named set of operations dispatchable by the runtime type of their first argument and extendable to types outside their original definition.

**Not:** A class hierarchy, a multimethod with an arbitrary dispatch function, or a compiler plugin.

**Relationships:** Protocol operations are Vars with native dispatch linkage; host types can participate through **platform adaptations** or library extensions.

## Multimethod

**Definition:** A named callable whose method selection uses an explicit dispatch function, dispatch values, hierarchy, and preferences.

**Not:** A protocol operation or Common Lisp generic function restricted to host type dispatch.

**Relationships:** A multimethod has a stable Var identity, native compiled method functions, and runtime-managed dispatch and cache state.

## Host bridge

**Definition:** The explicit mapping that makes a selected Common Lisp function, value, type, or adapter callable or usable with declared Common Clojure meaning.

**Not:** Automatic exposure of every host symbol, arbitrary private-IR access, or JVM interop emulation.

**Relationships:** Host bridges are an approved extension seam and are used to implement **platform adaptations** and adapted libraries.

**Scenarios:** A bridge exposes an SBCL thread primitive through a Clojure Var. A JVM class reference has no implicit meaning and requires an adaptation rather than a fabricated bridge.

## Platform adaptation

**Definition:** A deliberate replacement of a platform-specific Clojure facility or library assumption with behavior appropriate to Common Lisp and SBCL while preserving the relevant semantic contract.

**Not:** Silent acceptance of incompatible behavior or a claim of JVM binary/source compatibility.

**Relationships:** Platform adaptations normally use host bridges, protocols, and small runtime primitives; they are judged by **semantic compatibility**.

**Scenarios:** Channels may use SBCL synchronization facilities beneath Clojure-facing semantics. Java reflection tests are outside the target rather than being satisfied by placeholder objects.

## Semantic compatibility

**Definition:** Agreement with the selected observable Clojure language and library behaviors independent of JVM representation and Java-specific infrastructure.

**Not:** Passing every official JVM test, loading arbitrary Clojure projects unchanged, or reproducing Java classes and exceptions.

**Relationships:** Semantic compatibility is the conformance target for Common Clojure, persistent values, Vars, control flow, macros, protocols, and adapted libraries; its scope is owned by the [language and compatibility specification](specs/language-and-compatibility.md).

**Scenarios:** `nil` and `false` remain distinct and falsey on SBCL. A library that directly requires Java bytecode generation requires adaptation and is not automatically compatible.

## Adapted library

**Definition:** A library whose public Clojure-facing semantics are retained while platform-specific implementation assumptions are replaced for Common Clojure.

**Not:** An unmodified upstream library promised to work, or a compatibility shim that pretends JVM facilities exist.

**Relationships:** Adapted libraries exercise ordinary macros, protocols, persistent values, host bridges, and platform adaptations rather than private compiler IR.

## Legacy evaluator

**Definition:** The previous tree-walking execution path and its evaluator-specific environments, closures, dispatch, and compatibility stubs.

**Not:** The reader corpus, independently verified runtime functions, historical tests, or the new compiler's native-thunk evaluation operation.

**Relationships:** The legacy evaluator is isolated from the native compiler and runtime path by [Decision 0007](decisions/0007-replace-legacy-evaluator.md).

## Native vertical slice

**Definition:** A minimal end-to-end Common Clojure program that passes through the real reader, compiler, SBCL, artifact loading, and runtime ABI without legacy evaluation.

**Not:** A parser demo, emitted-but-uncompiled form, evaluator fallback, or broad feature milestone.

**Relationships:** A native vertical slice validates the architecture across one compiler path before the semantic surface grows.
