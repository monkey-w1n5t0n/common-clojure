# Architecture

How the Clojure-on-SBCL system works: components, relationships, data flows, and invariants.

## System Overview

This is a Clojure language implementation running on SBCL (Steel Bank Common Lisp). It has two phases:

1. **Read** — Clojure source text → CL data structures (via custom readtable)
2. **Eval** — CL data structures → values (via tree-walking interpreter)

There is no compilation phase, no JVM, and no bytecode generation.

## Component Diagram

```
Source (.clj)
    │
    ▼
┌──────────────────────────┐
│  Reader                  │  cl-clojure-syntax.lisp
│  (custom readtable)      │
│  Handles: [] {} #{} ^    │
│  #"" \char #tag regex    │
└──────────┬───────────────┘
           │ CL data (lists, vectors, hash-tables, symbols, keywords)
           ▼
┌──────────────────────────┐
│  Evaluator               │  cl-clojure-eval.lisp
│  (tree-walking)          │
│                          │
│  ┌─────────────────┐     │
│  │ clojure-eval    │─────┼── Dispatch on form type
│  │ (main dispatch) │     │
│  └────────┬────────┘     │
│           │              │
│  ┌────────┴────────┐     │
│  │ Special Forms   │     │── ~70 forms (if, def, fn, let, for, etc.)
│  │ (~line 8865)    │     │
│  └────────┬────────┘     │
│           │              │
│  ┌────────┴────────┐     │
│  │ Function App    │     │── apply-function (~line 9516)
│  │ (apply-function)│     │   dispatches: closure, vector, map, keyword, CL fn
│  └─────────────────┘     │
│                          │
│  ┌─────────────────┐     │
│  │ Core Functions  │     │── setup-core-functions (~line 3777)
│  │ (+, -, map,     │     │   registered in root env
│  │  reduce, etc.)  │     │
│  └─────────────────┘     │
│                          │
│  ┌─────────────────┐     │
│  │ Java Interop    │     │── eval-java-interop
│  │ (stubs only)    │     │   Math, System, Arrays, etc.
│  └─────────────────┘     │
└──────────────────────────┘
           │
           ▼
      Values (CL types)
```

## Data Flow: Evaluation

1. `eval-file` reads source, iterates forms, calls `clojure-eval` on each
2. `clojure-eval` dispatches on form type:
   - **Self-evaluating** (nil, numbers, strings, chars, keywords): return as-is
   - **Symbols**: look up in lexical env → var namespace → special rules (hex, Java interop, BigInt suffixes, constructors)
   - **Lists**: unwrap metadata, dispatch on head symbol name (special forms), else function application
3. Function application: eval head → eval args → `apply-function`

## Environment Model

```
env
├── vars: hash-table ("NS/NAME" → var struct)
│         Shared across all envs in the chain (same object)
├── bindings: alist ((name . value) ...)
│         New frame per scope (let, fn, loop)
├── parent: env or nil
│         Chain for lexical lookup
└── letfn-table: hash-table or nil
          For mutual recursion in letfn
```

- **Var lookup**: `env-get-var` checks `vars` hash-table with key `"NS/NAME"` (uppercased)
- **Lexical lookup**: `env-get-lexical` checks `bindings` → `letfn-table` → recurse to `parent`
- **Scoping**: `env-push-bindings` creates new env with old as parent

## Closure Model

Closures capture the `env` struct (not a copy). They share the `vars` hash-table with their defining environment. When called:
1. New env created extending closure's captured env
2. Parameters bound (with destructuring support)
3. Body forms evaluated sequentially
4. Last form's value returned

## Key Invariants

1. **Vars are namespace-qualified**: Always looked up as `"NS/NAME"` strings (uppercased)
2. **Maps are hash-tables**: Not sequences. Must explicitly convert to iterate.
3. **Vectors are simple-vectors**: Not sequences. Must explicitly convert.
4. **Keywords are self-evaluating**: Also act as functions (map lookup).
5. **NaN is incomparable**: Must guard all numeric comparisons with `sb-ext:float-nan-p`.
6. **Closures need wrapping**: Must use `ensure-callable` before passing to CL HOFs.

## Test Infrastructure

The official Clojure test suite uses:
- `deftest` — defines a test (body evaluated immediately)
- `is` — assertion, handles `(is expr)` and `(is (thrown? Type expr))`
- `are` — template-based multiple assertions (uses `do-template`)
- `testing` — context wrapper (no-op, just evaluates body)

Tests pass if `eval-file` completes without signaling a condition.
