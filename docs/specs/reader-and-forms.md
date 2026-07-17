---
stability: aspirational
layer: behavioural
audience: both
---

# Reader and Forms

> Spec: reading Common Clojure source into semantically lossless, source-located forms. Counterpart to [MAIN.md](MAIN.md).
> See also [value-model.md](value-model.md), [namespaces-vars-and-macros.md](namespaces-vars-and-macros.md), and [diagnostics.md](diagnostics.md).

## Source files

- `cl-clojure-syntax.lisp` — legacy reader experiment and syntax-case evidence only.
- `clojure-tests/reader.cljc` — cross-platform upstream-derived reader evidence.

---

## 1. Frame

1.1 The reader transforms Common Clojure text into Clojure data forms while preserving every distinction that can affect expansion, analysis, diagnostics, printing, equality, or execution.

**Why:** The compiler and macros must receive language data, not lossy Common Lisp reader approximations.

1.2 Semantic losslessness does not require retaining whitespace or comments after reading; it does require retaining origin and source span for every form and nested form.

**Why:** Formatting is not program meaning, while precise diagnostics and macro traces depend on locations.

1.3 Reading MUST NOT globally mutate the host Common Lisp readtable or package state.

**Why:** Embedded and ASDF builds must coexist safely with ordinary Common Lisp source.

---

## 2. Form ontology

2.1 Symbols and keywords are distinct Clojure values with case-preserving name and optional namespace components; they are not host package symbols or Common Lisp keywords.

**Why:** Host interning and case rules cannot express Clojure resolution, metadata, and equality faithfully.

2.2 Lists, vectors, maps, and sets retain their collection kind when used as forms. Map literals require an even form count, and set literals reject duplicate equal values.

2.3 Every form MAY carry metadata and MUST carry an origin descriptor sufficient to report source name, line, column, and containing span when it came from text.

2.4 `nil`, `true`, `false`, and the empty list read as the distinct values defined in [value-model.md §2.1](value-model.md).

---

## 3. Reading behavior

3.1 The reader MUST support Clojure list, vector, map, set, string, character, numeric, keyword, symbol, quote, syntax-quote, unquote, unquote-splicing, dereference, Var quote, metadata, anonymous-function, regex, discard, and tagged-literal syntax adopted by this corpus.

3.2 Commas are whitespace outside string and character literals. Semicolon comments and discard forms do not produce runtime forms.

3.3 Reader sugar MUST expand to ordinary source forms without resolving lexical or namespace meaning. Namespace resolution belongs to expansion and analysis.

**Why:** Reading must be deterministic from text and reader configuration alone.

3.4 Anonymous-function literals MUST infer `%`, `%N`, and `%&` bindings, reject nested anonymous-function literal ambiguity, and produce hygienic parameter symbols.

3.5 Metadata shorthand MUST normalize to a metadata map without evaluating metadata or the target form.

3.6 Reading multiple forms MUST preserve source order and distinguish a literal `nil` form from end of input.

**Why:** Using nil as an EOF sentinel makes valid source disappear.

---

## 4. Tagged literals and extensibility

4.1 Tagged literals resolve through a session-scoped declarative reader table whose handlers receive the already-read payload and return an ordinary Clojure value.

4.2 Unknown tags MUST signal a typed reader failure unless a configured default data-reader handles them.

4.3 A tagged-literal handler is an ordinary native function; adding one MUST NOT require access to compiler IR or a compiler plugin interface.

**Why:** Tagged data is a sufficient extension seam without exposing compiler internals.

---

## 5. Failure behavior

5.1 Unmatched delimiters, malformed tokens, invalid escapes, odd map forms, duplicate set values, illegal anonymous-function placeholders, and unknown tags MUST signal `reader-error` conditions as defined by [diagnostics.md §2.1](diagnostics.md).

5.2 A read failure MUST identify the smallest reliable source span and MUST NOT return a guessed or partial form as successful input.

5.3 Recovery for editor tooling MAY resume at a later form boundary, but recovered forms MUST remain marked as following an error and MUST NOT be compiled as though the source were valid.

---

## Open / Deferred

- **Should formatting-preserving concrete syntax trees be a separate tool-facing surface?** The language compiler needs semantic forms and spans, but future structural editors may need comments and whitespace.
- **Which tagged literals ship in the core distribution?** Decide from actual library adaptations rather than JVM precedent alone.
