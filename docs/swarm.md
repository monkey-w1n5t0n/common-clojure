# Swarm operating contract

**Workstream established:** 2026-07-17
**Ergo project:** `60407fcb`
**Native epic:** `f86d60db` — _Native Common Clojure on SBCL: one compiler path_

This document explains how multiple agents execute the native Common Clojure workstream.
It is not a duplicate backlog: task bodies, status, ownership, and dependency edges live
only in Ergo.

## Authority order

When evidence disagrees, use this order:

1. `docs/specs/MAIN.md` and its owning child spec define observable behavior.
2. Accepted records in `docs/decisions/` define hard-to-reverse architecture.
3. `docs/domain.md` defines what project terms mean.
4. The claimed Ergo task defines the authorized slice, ownership boundary, and evidence.
5. `ALIGNMENT.md` prioritizes strategic gaps.
6. Current source, old tests, branches, and imported tasks are evidence only.

Changing a higher authority requires updating it explicitly; an implementation must not
silently override it.

## Find work

The epic is a coordination container and is blocked behind its final cutover gate. Do not
claim it. The only initially ready native task is `ca3ce848`, which creates the module
scaffold and safe ownership seams.

`ergo ready` has no `--part-of` filter. Inspect both views and choose only an ID present
in the native epic:

```bash
ergo ready --json
ergo list --status open --part-of f86d60db --json
ergo show <matching-id>
ergo claim <matching-id>
```

The 88 open imported evaluator-era tasks are hidden from normal queues. Never add
`--all` for this swarm. They remain untouched migration evidence; neither completing nor
dismissing them would truthfully describe the new architecture.

## Dependency and parallelism model

The graph deliberately starts serially. The scaffold allocates modules and central
interfaces; value ABI, compiler-session state, and the honest test harness then establish
independent ownership. Reader and compiler-core work converge through semantic analysis,
SBCL emission, native macro staging, and validated `recur` into the N10 vertical-slice
gate.

No broad runtime or library work is ready before N10. Once that gate passes, independent
owners can work on equality/collections, module semantics, diagnostics, interop,
protocols/types, multimethods, and concurrency. Collection and adapter integration are
serialized again. Conformance and performance gates verify the assembled core without
opportunistically editing other owners' modules. Library adaptations own isolated
`ports/` trees and feed missing core behavior back through discovered Ergo tasks.

The final cutover remains blocked by conformance, performance, `core.match`,
`clojure.spec.alpha`, O'Doyle Rules, and `core.async` evidence. Ergo does not provide an
epic rollup or graph command, so inspect gate tasks directly before closing the epic.

## Ownership rules

- Claim before editing. One active task owns each named module/file family.
- Central ASDF, package, and public-seam edits belong to the scaffold or an explicitly
  named integration gate.
- A task owns only the minimal `MAP.md` inventory update or `ALIGNMENT.md` defect removal
  made necessary by its authorized behavior, and lands it in the same commit. Coordinate
  before touching these shared files; broader reconciliation belongs to an explicitly
  named integration gate.
- A task may consume another module's declared seam but must not “help” by editing that
  module concurrently.
- When a seam is insufficient, record the concrete conflict and create a dependency-wired
  discovered task. Do not add a backdoor, duplicate path, or compatibility shim.
- Gate owners add reproductions and reports. They return failures to the responsible
  module instead of making cross-cutting fixes under the gate.
- Library agents may not edit private compiler IR. A repeated unmet need from two real
  ports is evidence for a new decision, not automatic permission for a compiler hook.

## Per-task completion

Before implementation, read every cited spec section and relevant decision in full.
Acceptance uses the public seam named by the task and includes negative evidence where
required: no evaluator dependency, no swallowed failure, no partial publication, and no
unexpected allocation or dynamic dispatch in a promised native fast path.

Before closing:

- run the exact focused and broader gates named in the task;
- record commands, SBCL/runtime context, and observable results;
- update affected specs, decisions, `MAP.md`, `ALIGNMENT.md`, and deeper docs in the same
  commit;
- leave `CLAUDE.md -> AGENTS.md` intact;
- verify no user-owned untracked diagnostic file was changed;
- close with a reason that names the evidence, not merely “implemented.”

If a requested change would alter compatibility scope, the public seam, runtime ABI,
persistent representation exposure, security/host policy, or an accepted decision, stop
at a design wall. State the intended outcome, conflicting assumptions, invalidated
assumption, smallest coherent alternative, decision required, and current safe state.

## Non-negotiable negative space

No task in this epic authorizes runtime source/AST walking, evaluator environments,
interpreted closure bodies, `clojure-eval` fallback, silent JVM stand-ins, public compiler
IR, arbitrary emitter callbacks, a hypothetical backend interface, weakened assertions,
or raw mutable host collections exposed as persistent Clojure values.

The legacy reader and evaluator may be read as described in
[the legacy assessment](legacy-assessment.md). Reuse means transplanting one verified
behavior behind the new native seam; it never means sharing the old execution path.
