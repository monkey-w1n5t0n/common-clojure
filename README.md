# Clojure on SBCL

A Clojure implementation built on top of Steel Bank Common Lisp (SBCL).

## Overview

This project aims to implement Clojure on SBCL, using the official Clojure test suite as our north star for correctness and completeness. We're following Test-Driven Development, tracking progress with beads issues, and working through features in a logical dependency order.

## Goal

Make all 68 Clojure core test files pass. These tests cover:
- Reader (parsing)
- Core evaluation
- Collections (vectors, maps, sets, sequences, transducers)
- Concurrency (refs, atoms, agents, volatiles)
- Namespaces
- Metadata
- Protocols and multimethods
- Java interop
- And more...

## Current Status

**Phase: Foundation - Reader Implementation**

Currently implementing the reader layer. The parser can read Clojure's vector `[...]` and map `{...}` literals, but needs significant expansion:

- [ ] Keywords (`:foo`, `:ns/foo`, `::auto-resolve`)
- [ ] Symbols and namespace-qualified symbols
- [ ] Numbers (ints, floats, ratios, BigInt, radix notation)
- [ ] Set literals (`#{...}`)
- [ ] Quote/syntax-quote (`'`, `` ` ``, `~`, `~@`)
- [ ] Metadata (`^{:foo 1}`)
- [ ] Var quote (`#'foo`)
- [ ] Anonymous function literals (`#(...)`)
- [ ] Regex literals (`#"\d+"`)
- [ ] Character literals (`\a`, `\newline`, etc.)
- [ ] Dispatch literals (`#inst`, `#uuid`)
- [ ] Comments (`;`, `(comment ...)`, `#_`)

## Development

### Tracking Progress

We use [beads](https://github.com/monadplus/beads) for issue tracking with 93 tasks organized in dependency chains:

```bash
# See what's ready to work on (no blockers)
bd ready

# Show the foundation issue (start here)
bd show common-clojure-wxt

# Mark work in progress
bd set-state in-progress common-clojure-wxt

# Complete and close
bd close common-clojure-wxt
```

### Running Tests

```bash
# Run the Clojure test suite scanner
sbcl --script test-runner.lisp

# Run existing Common Lisp tests
sbcl --script tests.lisp
```

The test runner shows:
- Which test files exist and what features they need
- Parse status (can we read the file?)
- Categorization by feature layer (Reader, Eval, Collections, etc.)

### Project Structure

```
├── clojure-tests/          # Official Clojure test suite (68 files)
├── test-runner.lisp        # Test scanner/runner
├── cl-clojure-syntax.lisp  # Reader implementation
├── cl-clojure-syntax.asd   # ASDF definition
├── package.lisp            # Package definition
└── tests.lisp              # Original Common Lisp tests
```

## Implementation Layers

Features are implemented in dependency order:

1. **Reader Layer** - Parse Clojure syntax into Lisp data structures
2. **Eval Layer** - Evaluate forms (def, fn, if, let, etc.)
3. **Namespace Layer** - ns, require, use, import
4. **Collection Layer** - Persistent data structures
5. **Concurrency Layer** - Refs, atoms, agents, STM
6. **Metadata Layer** - with-meta, meta, reader metadata
7. **Macro Layer** - defmacro, core macros
8. **Protocol Layer** - defprotocol, extend-type, reify
9. **Multimethod Layer** - defmulti, defmethod
10. **Java Interop Layer** - gen-class, proxy, arrays
11. **Printer Layer** - pr-str, pprint, EDN
12. **REPL Layer** - Read-eval-print loop

## Contributing

Work follows the beads dependency chain:

1. Run `bd ready` to see available work
2. Pick up a task: `bd set-state in-progress <issue>`
3. Implement the feature
4. Run tests to verify
5. Close: `bd close <issue>`

Use Conventional Commits:
```
feat(reader): implement keyword parsing
fix(eval): handle nil in if conditional
test: add vector equality tests
```

## License

MIT

## Author

Droid
