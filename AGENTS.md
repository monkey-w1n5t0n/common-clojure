# Agent Instructions — Common Clojure

`AGENTS.md` is the canonical project prompt. `CLAUDE.md` must remain the relative
symlink `CLAUDE.md -> AGENTS.md`; never maintain a second copy.

## Mission

Build a seamless Clojure semantic layer that compiles through Common Lisp to native
SBCL code. The language is for new Common Clojure applications and deliberately adapted
libraries. It is not a JVM emulator and it is not a tree-walking interpreter.

The observable contract lives in `docs/specs/`. Existing source, tests, branches, and
old tasks are evidence only when they agree with that contract.

## Read before changing code

Read these in order:

1. `MAP.md` — repository inventory and spec discovery.
2. `docs/specs/MAIN.md` and every child spec cited by the task.
3. Relevant accepted records in `docs/decisions/`.
4. `docs/domain.md` — canonical vocabulary.
5. `docs/swarm.md` — authority, work selection, ownership, and design-wall rules.
6. The full body and dependency edges of the claimed Ergo task.
7. `ALIGNMENT.md` — current strategic defects.

When evidence conflicts, authority descends from specs, to accepted decisions, to domain
language, to the claimed task, to `ALIGNMENT.md`, and finally to current source, old
tests, branches, and imported tasks. `MAP.md` is a navigation aid, not a higher authority.

If source or a legacy test contradicts a spec, do not preserve both paths or weaken the
test. Surface the conflict and implement the smallest coherent design that satisfies the
specification and accepted decisions.

## Non-negotiable architecture

- Runtime execution contains no AST/form walking, evaluator environment lookup,
  interpreted closure body, or fallback to `cl-clojure-eval:clojure-eval`.
- All source execution goes through one pipeline: read, macroexpand, analyze into a
  private semantic IR, emit Common Lisp, and ask SBCL to compile it.
- A REPL or `eval-string` compiles a native thunk. ASDF, file compilation, loading, REPL,
  and embedding are adapters over the same compiler seam, never alternate evaluators.
- SBCL is the sole backend. Do not add a generic backend abstraction without a new
  accepted decision based on a real second backend.
- The compiler IR is private. Ordinary Clojure macros, tagged literals, protocols over
  host types, declarative Common Lisp bridges, and small CL adapter modules are the
  extension surfaces. A compiler hook requires evidence from at least two concrete
  library ports and a new accepted decision.
- Preserve Clojure's observable value semantics: nil and false are distinct falsey
  values; the empty Clojure list is distinct from nil and truthy; equality and hashing
  are Clojure-semantic rather than raw Common Lisp `equal`.
- Clojure namespaces and Vars form a semantic registry. Backing CL packages are code
  generation, storage, and interop machinery, not the name resolver.
- Default development compilation preserves Var redefinition. Direct linking or
  inlining that changes redefinition behavior is explicit final/sealed optimization.
- `recur` is checked for target, tail position, and arity before host compilation and is
  lowered to bounded-stack native control flow with simultaneous argument reassignment.
- A normal application artifact may depend on the runtime ABI but not on the compiler,
  reader, test harness, or legacy evaluator.

## Compatibility boundary

Clojure language semantics are the design reference. The vendored official tests are a
reference corpus, not the North Star and not an acceptance oracle. JVM classes,
bytecode-specific machinery, and Java interop calls must receive an explicit SBCL/Common
Lisp adaptation or a typed, source-located failure at the earliest reliable phase; never
add silent stubs.

Priority library targets are `core.match`, `clojure.spec.alpha`, O'Doyle Rules, and an
adapted `core.async`. Their ports are milestone evidence. They do not imply that arbitrary
Clojure projects must run unchanged.

## Work through Ergo

Use only native-compilation epic `f86d60db` described in `docs/swarm.md` unless the user
explicitly redirects you. The imported evaluator-era tasks are frozen migration evidence;
they are hidden from normal Ergo queues and must not be claimed just because `--all`
reveals them.

```bash
cd /home/w1n5t0n/src/common-clojure
ergo ready --json
ergo list --status open --part-of f86d60db --json
ergo show <matching-id>
ergo claim <matching-id>
```

Claim only an ID present in both views. Never claim the epic itself. `ergo ready` is a
global queue and may contain unrelated projects; `--all` additionally reveals frozen
migration evidence and is forbidden for native-work selection.

Before implementation, confirm that all blockers are closed, read the exact cited spec
sections, and keep to the task's file-ownership boundary. If another active task owns a
shared compiler module, coordinate through a narrower seam instead of editing it
concurrently.

Close work only with observable evidence:

```bash
ergo done <id> --reason "Implemented; <commands/evidence>; specs synchronized"
```

Create newly discovered coding work in Ergo and wire real dependencies. Do not use
Markdown task lists, TODO files, or the retired tracker. Do not close, kill, or rewrite
the imported backlog without explicit authorization.

## Verification ladder

Use the narrowest public seam that proves the behavior, then run every broader gate the
task names:

1. Reader/form and analyzer tests, including source-located failure cases.
2. Runtime semantic tests for values, equality, collections, Vars, and dispatch.
3. Fresh-process compilation tests that load only the runtime and produced FASL, with the
   compiler, reader, test harness, and legacy evaluator absent.
4. Common Lisp calls proving exported values are SBCL compiled functions.
5. Disassembly/allocation/performance checks for promised native fast paths.
6. Adapted-library conformance tests when a milestone reaches that layer.

`./tests.sh` is a legacy characterization harness. A file loading without a serious
condition, an eager `deftest` body, or a false `is` expression that merely returns nil is
not a passing test.

Never weaken an assertion, swallow a compiler condition, or add an interpreter fallback
to make a gate green.

## Legacy code policy

- `cl-clojure-syntax.lisp` contains reader algorithms and cases worth mining, but its
  representations are not the new form/value ABI.
- `cl-clojure-eval.lisp` is a semantic quarry only. Selectively port verified pure
  algorithms behind new tests; never link its evaluator, environment, or closure path
  into the compiler/runtime.
- `feat/clojure-on-common-lisp` is an architectural sketch, not a merge base. Manually
  mine useful ideas only after they satisfy the current specs.
- Do not edit or delete the user's untracked diagnostic scripts unless a task explicitly
  scopes them in.

## Documentation is part of done

Before every commit, check `MAP.md`, `ALIGNMENT.md`, the task's cited specs, linked
decisions, and deeper docs. The task owns the minimal documentation edits made necessary
by its authorized behavior, including the corresponding `MAP.md` inventory line or the
deletion of a resolved `ALIGNMENT.md` defect. Make those edits in the same commit as the
behavior and coordinate before touching these shared files; broader map/alignment cleanup
belongs to an explicitly named integration task. Do not turn `ALIGNMENT.md` into a
changelog.

Specs describe durable behavior, decisions explain hard-to-reverse choices, Ergo holds
coding tasks, and `MAP.md` only says where things are. Keep those roles separate.

Use Conventional Commit messages with a focused scope, for example:

```text
feat(compiler): emit native fixed-arity functions
fix(runtime): distinguish false from nil in equality
test(compiler): reject non-tail recur with a source span
docs(specs): define native artifact isolation
```
