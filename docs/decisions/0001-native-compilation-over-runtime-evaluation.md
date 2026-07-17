# Native compilation replaces runtime tree walking

- **Date:** 2026-07-17
- **Status:** Accepted

## Context

The project exists to combine Clojure semantics with Common Lisp and SBCL's compiler, runtime, debugging, and deployment infrastructure. The legacy evaluator executes source forms through a tree walker. Extending that evaluator can increase surface compatibility, but it cannot make ordinary application functions native SBCL functions and encourages semantic fixes to accumulate in evaluator dispatch.

## Decision

Common Clojure programs are macroexpanded, semantically analyzed, lowered to Common Lisp, and compiled by SBCL before execution. Ahead-of-time compilation produces native artifacts; interactive evaluation compiles and invokes a native thunk. The runtime execution path does not call the legacy tree-walking evaluator.

The project targets Clojure semantics on SBCL, not runtime interpretation of Clojure syntax and not JVM emulation.

## Alternatives considered

- Continue deepening the tree-walking evaluator. This preserves existing code but misses the native-performance and host-integration purpose of the project.
- Keep an interpreter for development and add a compiler for releases. This creates two semantic implementations whose behavior and bugs can diverge.
- Compile only selected hot functions and interpret the rest. This makes execution strategy observable, complicates calls and debugging, and leaves a permanent fallback path.

## Consequences

- User functions can become ordinary native compiled functions and use SBCL tooling.
- Every supported execution surface incurs compilation, including the REPL.
- Compiler and runtime semantics must be implemented explicitly instead of inherited from the evaluator.
- Legacy code may be mined for verified behavior, but evaluator execution is not a supported fallback.
- This decision would be revisited only if native compilation ceased to be the project's defining outcome.

## References

- [Language and compatibility specification](../specs/language-and-compatibility.md)
- [Common Clojure](../domain.md#common-clojure)
- [Native artifact](../domain.md#native-artifact)
- [Legacy evaluator](../domain.md#legacy-evaluator)
- [Decision 0007: Replace the legacy evaluator rather than layering over it](0007-replace-legacy-evaluator.md)
- [Decision 0010: All execution surfaces share one compiler path](0010-one-compiler-path.md)
