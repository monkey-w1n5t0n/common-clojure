# Implementation Patterns

Patterns and conventions for implementing Clojure features in Common Lisp.

## Architecture Overview

The system has two main components:
1. **Reader** (`cl-clojure-syntax.lisp`, ~843 lines) — Parses Clojure syntax into CL data
2. **Evaluator** (`cl-clojure-eval.lisp`, ~9663 lines) — Evaluates CL data to produce values

### Core Data Structures
- **`var`** — Global bindings, keyed by `"NS/NAME"` (uppercased) in env's vars hash-table
- **`env`** — Lexical scope: `vars` (hash-table), `bindings` (alist), `parent` (chain), `letfn-table`
- **`closure`** — Functions: `params`, `body`, `env` (captured), `name`, `macro-p`
- **`lazy-range`** — Lazy number ranges with `start`, `end`, `step`, `current`
- **`delay`** — Delayed computation with `thunk`, `value`, `forced-p`

### Collection Representation
- Vectors = CL `simple-vector`
- Maps = CL `hash-table` (with `equal` test)
- Sets = `(set items...)` lists
- Lists = CL lists (cons cells)

### Main Dispatch: `clojure-eval` (~line 8865)
1. Self-evaluating: nil, numbers, strings, characters, keywords, booleans
2. Symbols: nil/true/false → lexical bindings → var lookup → special rules
3. Lists: ~70 special forms dispatched by head name, else function application

### Function Application: `apply-function` (~line 9516)
Dispatches on: closure (bind params + eval body), vector (index lookup), hash-table (key lookup), keyword (self-lookup), CL function (apply).

## Adding Functions

1. Define in `cl-clojure-eval.lisp` with `clojure-` prefix
2. Add forward declaration at top of file
3. Register in `setup-core-functions` (~line 3777)

## Adding Special Forms

1. Add evaluator function (e.g., `eval-my-form`)
2. Add dispatch case in `clojure-eval` (~line 8865)

## Adding Namespace Functions

Add case in `eval-java-interop`:
```lisp
((string-equal class-name "namespace-name")
 (cond
   ((string-equal member-name "function-name") ...)
   ...))
```

## Critical Gotchas

1. **Symbol comparison**: Use `string=` not `eq` (different packages: `cl-clojure-syntax` vs `cl-clojure-eval`)
2. **Closure wrapping**: Use `ensure-callable` before passing to CL `funcall`/`mapcar`/`every`
3. **NaN handling**: Use `sb-ext:float-nan-p` before any numeric comparison; NaN should be incomparable
4. **Metadata format**: `(meta-wrapper value metadata)` — value is `cadr`, metadata is `cddr`
5. **Vector vs List**: Vectors are `simple-vector`, check with `typep x 'simple-vector`
6. **Hash-table as map**: Not a sequence — must explicitly convert via `clojure-seq` or `hash-table-alist`
7. **Case**: Var lookup uppercases symbol names; keyword names are preserved as-is
8. **`and`/`or`**: These are special forms in dispatch; when used as values (e.g., `(reduce and ...)`), they need a function wrapper registered in core functions

## File Reference

| File | Lines | Purpose |
|------|-------|---------|
| `cl-clojure-syntax.lisp` | ~843 | Reader: parses Clojure syntax |
| `cl-clojure-eval.lisp` | ~9663 | Evaluator: all functions, special forms, core library |
| `cl-clojure-transducers.lisp` | ~424 | Transducer protocol support |
| `cl-clojure-case.lisp` | ~50 | Case special form |
| `package.lisp` | ~70 | Package definitions |
| `clojure-tests/*.clj` | ~102 files | Official Clojure test suite |
| `run-tests.lisp` | ~70 | Test runner |
