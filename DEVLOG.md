# DEVLOG - Clojure on SBCL Development Log

## Project Status

**Goal:** Implement Clojure evaluation on SBCL. The reader parses all 68 test files.

**Current Status:**
- Reader: ✅ Complete - All 68 test files parse successfully
- Eval: ⚠️ Skeleton exists - 4/68 test files evaluate without errors
- Heap exhaustion issue fixed! Tests now run in ~9 seconds instead of crashing
- Many core functions and special forms are stubbed or incomplete

## Recent Work

### Iteration 1 - 2025-01-17

**Focus:** Fix critical bugs and implement core functions

**Changes Made:**
1. **Fixed `clojure-fnil` bug** - undefined variable `FILLED-ARGS`
   - Location: cl-clojure-eval.lisp:1226-1239
   - The loop was using `finally` clause incorrectly, trying to reference `filled-args` before it was returned
   - Fixed by using a simpler approach with `fill-count` and explicit append

2. **Implemented `map` function** - was returning '()
   - Location: cl-clojure-eval.lisp:1000-1049
   - Now handles single and multiple collection mapping
   - Supports lazy ranges, lists, and vectors

3. **Implemented `comp` function** - function composition
   - Returns identity when called with no args
   - Composes functions right-to-left (Clojure style)

4. **Implemented `juxt` function** - applies multiple functions to same args

5. **Implemented `when` and `when-not` special forms**
   - Had to rename to `eval-clojure-when` to avoid conflict with CL's `eval-when` special form

6. **Implemented `are` test macro**
   - Simplified implementation using loop instead of do
   - Still has issues with infinite sequences (heap exhaustion)

7. **Implemented metadata support:**
   - `meta` - get metadata from object
   - `with-meta` - attach metadata to object
   - `vary-meta` - transform metadata
   - Used wrapper cons cell pattern: `(meta-wrapper metadata value)`
   - Updated `apply-function` to unwrap values

8. **Implemented set literal handling**
   - Reader returns `(set elements...)` for `#{...}`
   - Evaluator now converts to hash table

9. **Implemented map operations:**
   - `get` - get value from map/collection with optional default
   - `contains?` - check if key exists
   - `assoc` - add/update key-value pairs
   - `dissoc` - remove keys
   - `keys` - get all keys
   - `vals` - get all values

**Known Issues:**
- Heap exhaustion when running tests - related to `are` macro processing infinite lazy ranges
- Many Java interop features are missing (Class/TYPE, .method, new, etc.)
- Test helper functions like `thrown?`, `fails-with-cause?` are stubs

**Test Results:**
- Parse: 68 ok, 0 errors ✅
- Eval: 6 ok, 62 errors
- Compilation warnings resolved

**Next Steps:**
1. Implement stubs for Java interop symbols
2. Implement or stub `clojure.template/do-template` macro
3. Add more core functions as tests require them

---

### Iteration 2 - 2025-01-17

**Focus:** Fix heap exhaustion and doseq bug

**Changes Made:**

1. **Fixed heap exhaustion in `clojure-vec`** - cl-clojure-eval.lisp:1026-1037
   - The function was calling `lazy-range-to-list` without a limit parameter
   - For infinite lazy ranges, this would try to materialize the entire range, causing heap exhaustion
   - Fixed by adding a limit (10000 elements) for infinite ranges

2. **Fixed `eval-are` to handle lazy ranges** - cl-clojure-eval.lisp:1634-1663
   - Changed to safely calculate arg-count for lazy ranges (bounded vs infinite)
   - Added iteration limit (10000) to prevent infinite loops
   - Convert args-vec to list safely with limit for infinite ranges

3. **Fixed critical bug in `eval-doseq`** - cl-clojure-eval.lisp:480-493
   - Line 486 was using `(cadr bindings-body)` which only got the FIRST body expression
   - This meant multi-body `doseq` forms would lose all but the first expression
   - Changed to `(cddr bindings-body)` to correctly get ALL body expressions
   - Bug was causing `(are ...)` forms inside `doseq` to not be evaluated

**Errors Fixed:**
- Heap exhausted during garbage collection - FIXED ✅
- Tests now complete in ~9 seconds instead of crashing

**Test Results:**
- Parse: 68 ok, 0 errors ✅
- Eval: 4 ok, 64 errors (heap exhaustion eliminated, some tests now show different errors)
- The "numbers" test now correctly evaluates past `are`, but fails on `do-template` (a macro from `clojure.template`)

**Next Steps:**
1. Implement or stub `clojure.template/do-template` macro
2. Implement stubs for Java interop symbols
3. Add more core functions as tests require them

---
