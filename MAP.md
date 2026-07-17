# Project Map

## Front doors

- `AGENTS.md` — canonical project instructions and architecture guardrails for every coding agent.
- `CLAUDE.md` — tracked relative symlink to `AGENTS.md`; never a separate prompt.
- `README.md` — human-facing mission, status, and orientation.
- `ALIGNMENT.md` — dated, prioritized critique of the gaps between current code and mission.

## Specs

- **Root**: `docs/specs/`
- **Entry**: `MAIN.md`
- **Layout**: `flat`
- **Index**: `docs/specs/.index.json` (auto-generated, git-tracked)
- **Skill**: invoke `/specs` to review, maintain, add, or navigate

### Local conventions

- The corpus is a recreation-oriented contract: focused child specs own normative promises, while `MAIN.md` remains a thin navigation anchor.
- Current source paths listed by specs are legacy evidence unless a spec explicitly says otherwise.
- Architecture is binding only where the externally observable native-SBCL contract requires it.

### Exclusions

- `_archive/`
- `_reviews/`
- `_drafts/`

### Lint policy

- `validators: [reference-integrity, frontmatter-integrity, layer-stability-consistency]`

### How to use specs in this project

Before changing behaviour, find the owning claim via `/specs navigate`. The spec wins over the implementation by intent; when intent changes, update the owning spec in the same commit as the behavioural change.

## Design and coordination

- `docs/domain.md` — precise ubiquitous language for compiler, runtime, hosting, and compatibility concepts.
- `docs/decisions/` — Accepted architecture decisions and rejected alternatives; sequential records 0001–0010 define the pivot.
- `docs/swarm.md` — canonical Ergo epic, read order, ownership boundaries, and multi-agent operating contract.
- `docs/legacy-assessment.md` — dated evidence inventory for main and `feat/clojure-on-common-lisp`; says what may be mined and what must be rejected.

## Current implementation — legacy evidence

- `cl-clojure.asd` — shipped legacy reader/evaluator system; not a conforming native compiler system.
- `cl-clojure-syntax.asd` — syntax-only legacy ASDF system.
- `package.lisp` — legacy reader/evaluator package surface.
- `cl-clojure-syntax.lisp` — readtable and syntax experiment; useful cases, incompatible form/value representation.
- `cl-clojure-eval.lisp` — 9,901-line tree walker and semantic quarry; forbidden as a native-path dependency.
- `cl-clojure-case.lisp` — evaluator-era case support.
- `cl-clojure-transducers.lisp` — evaluator-era sequence/transducer implementation.
- `sbcl-init.lisp` — legacy SBCL compiler-policy experiment; useful performance evidence only.

No conforming `src/` compiler/runtime layout exists yet. Ergo task `ca3ce848` owns that
scaffold and must update this map when it allocates modules.

## Tests and historical evidence

- `clojure-tests/` — upstream-derived JVM Clojure corpus; source of semantic cases, not the completion oracle.
- `run-tests.lisp` and `tests.sh` — legacy load-survival harness; cannot prove assertions or native execution.
- `tests.lisp` and top-level `test_*.clj` — historical/ad hoc reader/evaluator examples.
- `DEVLOG.md` — evaluator-era development history, not current status.
- `check-parens.lisp` and `clojure-test-fixer-prompt.md` — legacy maintenance helpers/context.

## Conventions and gotchas

- The native workstream is Ergo epic `f86d60db`; never use `--all` when selecting its work.
- A normal application FASL must load with the runtime only; compiler, reader, tests, and evaluator are build-time/evidence systems.
- `feat/clojure-on-common-lisp` is a sketch to mine manually, not a merge base.
- User-owned untracked diagnostic scripts are outside normal task scope.
- Strategic smells and accepted debt live only in `ALIGNMENT.md`; coding work lives only in Ergo.
