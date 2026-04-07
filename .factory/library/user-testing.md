# User Testing

Testing surface and validation approach for the Clojure-on-SBCL mission.

## Validation Surface

- **CLI**: Run `sbcl --script run-tests.lisp` to execute the full Clojure test suite
- **Output format**: `=== Passed Tests ===` section listing names, `=== Failed Tests ===` section, then `Total: X passed, Y failed`
- **Exit code**: 0 if all pass, 1 if any fail
- **No browser, no API, no external services needed**

## How Tests Work

The test runner (`run-tests.lisp`) evaluates each `.clj` file in `clojure-tests/` using `eval-file`. A test "passes" if the file evaluates without errors (no condition thrown). The official Clojure test suite uses `deftest`, `is`, `are` macros for assertions — these are all handled as special forms in the evaluator.

## Validation Approach

1. Run `sbcl --script run-tests.lisp 2>&1 | tail -50`
2. Check that target test files appear in "Passed Tests" section
3. Verify no regressions: all 68 baseline tests still pass
4. Verify target tests are no longer in "Failed Tests"

## Resource Classification

- **Max concurrent validators: 1** — Single SBCL process, no concurrency needed
- **Runtime**: ~30 seconds for full suite
- **Memory**: ~200MB SBCL heap
- **CPU**: Single core, minimal load

## Test Exclusions (14 — Java Interop, always in "Failed Tests")

These are permanently excluded and expected to fail:
clojure_xml, evaluation, genclass, java_interop, main, method_thunks, param_tags, predicates, reflect, rt, serialization, streams, string, transducers

## Baseline Passing Tests (68 — MUST NOT regress)

annotations, api, atoms, clojure_set, clojure_zip, control, data, data_structures_interop, debug_deftest, debug_math, def, delays, edn, fn, generated_all_fi_adapters_in_let, generated_functional_adapters_in_def, generated_functional_adapters_in_def_requiring_reflection, generators, keywords, logic, macros, math, math_copy, math_exact, math_exact2, math_exact3, math_exact4, math_exact5, math_exact6, math_exact7, math_funcs, math_no_header, math_no_ns, math_with_is, math_with_test, ns_libs, ns_libs_load_later, numbers, parallel, pprint, refs, repl, run_single_test, server, test_anon_fn, test_both, test_call, test_concat_pattern, test_create_vals, test_drop_last, test_full_pattern, test_let_3entries, test_let_4entries, test_let_concat, test_let_create_vals, test_let_few, test_let_intvecs, test_let_three, test_m_pi, test_m_sin, test_persistent_list, test_vec_debug, test_vector_of, test_zipmap_debug, transients, try_catch, vars, volatiles
