---
stability: aspirational
layer: behavioural
audience: both
---

# Functions and Control Flow

> Spec: function values, invocation, lexical closures, kernel forms, exceptions, and native loop/recur. Counterpart to [MAIN.md](MAIN.md).
> See also [value-model.md](value-model.md), [collections-and-sequences.md](collections-and-sequences.md), and [performance.md](performance.md).

## Source files

- `cl-clojure-eval.lisp` — legacy function and special-form semantic notes; forbidden execution architecture.
- `cl-clojure-case.lisp` — legacy evaluator-specific case implementation.
- `clojure-tests/control.clj` — host-independent control-flow evidence.
- `clojure-tests/fn.clj` — function and arity evidence.

---

## 1. Frame

1.1 Common Clojure functions and closures execute as SBCL-compiled native functions.

**Why:** A closure that retains source bodies and evaluator environments would violate the native execution contract even if its outer wrapper were compiled.

1.2 Evaluation order, arity behavior, truthiness, exception flow, and tail recurrence follow this spec regardless of AOT or interactive entry.

---

## 2. Function values and invocation

2.1 A function expression produces a compiled callable that captures lexical values according to lexical scope; it does not capture a runtime map of source names to unevaluated bodies.

2.2 Fixed, variadic, named-recursive, and multi-arity functions dispatch by Clojure arity rules. Duplicate arities and multiple variadic overloads are analysis errors.

2.3 Calling with no matching arity signals a typed arity condition containing callable identity when known, supplied count, and supported arities.

2.4 Function position and arguments evaluate exactly once, from left to right, before invocation unless a special form or macro explicitly defines conditional evaluation.

2.5 Locals shadow Vars. Local function calls use native lexical calls when statically known; unknown callable values use the Clojure invocation protocol.

2.6 Functions, Vars containing callables, keywords, maps, sets, and vectors participate in invocation with their Clojure lookup/default behavior as detailed in [collections-and-sequences.md §4.1](collections-and-sequences.md). Other values signal a typed not-callable condition.

---

## 3. Bindings and destructuring

3.1 Lexical binding initializers evaluate in source order with each binding visible only where the selected binding form specifies.

3.2 Sequential, associative, nested, rest, `:keys`, `:syms`, `:strs`, namespaced, `:or`, and `:as` destructuring MUST preserve Clojure binding and missing-value behavior.

3.3 Destructuring is compiled into native access and binding operations; it MUST NOT invoke the legacy evaluator on a synthesized form.

**Why:** Destructuring is pervasive in target libraries and must not create a hidden interpreted path.

3.4 Function preconditions and postconditions, when supported through macros, execute with ordinary truthiness and signal assertion conditions with source context.

---

## 4. Kernel and macro-defined forms

4.1 The primitive compiler kernel contains only forms requiring analyzer control: definition, conditional branch, sequencing, lexical binding, function construction, loop/recur, quote, Var quote, throw, try/catch/finally, mutation of permitted targets, and explicit host bridge forms.

4.2 `if` evaluates only the selected branch and uses [value-model.md §2.2](value-model.md) truthiness.

4.3 `do` evaluates forms left to right and returns the last result, or nil when empty.

4.4 `try` runs at most one matching catch and always runs `finally` on normal or non-local exit; a `finally` non-local exit supersedes the pending result or condition.

4.5 `and`, `or`, `when`, threading forms, `cond`, `case`, `let`, `defn`, comprehensions, resource helpers, and comparable higher forms SHOULD be macros over the kernel.

**Why:** Ordinary macros make the language adaptable while keeping the compiler seam small.

---

## 5. Loop and recur

5.1 `recur` is valid only in tail position of the nearest function arity or loop target and MUST supply exactly that target's binding count.

5.2 Illegal position, crossing a `try`/`finally` protection region, target ambiguity, or arity mismatch is an `analysis-error`; no runtime recur stub exists.

**Why:** Static rejection prevents miscompiled control flow and makes bounded-stack behavior dependable.

5.3 Recur argument expressions evaluate exactly once, left to right, into temporaries before target bindings change; assignment is observably simultaneous.

5.4 Valid recur lowers to native control flow and runs in bounded stack space. A typed fixnum loop meeting [performance.md §3.1](performance.md) MUST allocate no storage in its steady-state recur path.

5.5 Function-position self-recur and explicit loop recur share the same validation and simultaneous-rebinding semantics.

---

## 6. Non-local exits and conditions

6.1 `throw` raises the supplied Clojure or bridged host condition without converting it into a return value.

6.2 Catch matching uses declared Common Clojure type/condition relationships, including explicitly bridged CL condition classes; it does not emulate Java class names.

6.3 Dynamic bindings, resource cleanup, and finally clauses restore their state during all supported non-local exits.

6.4 Runtime stack information SHOULD name Clojure namespace/functions and source spans rather than only generated host symbols, as specified by [diagnostics.md §3.1](diagnostics.md).

---

## Open / Deferred

- **Which non-local control constructs beyond throw/try belong in the stable language?** Add only with a real Common Lisp integration or library consumer.
- **Should ordinary self-tail calls be optimized when not written as `recur`?** Such optimization must not weaken recur's explicit guarantees or diagnostics.
