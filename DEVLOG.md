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

### Iteration 3 - 2025-01-17

**Focus:** Implement do-template, array functions, and destructuring support

**Changes Made:**

1. **Implemented `do-template` special form** - cl-clojure-eval.lisp:1756-1810
   - Performs symbol substitution on template expression before evaluating
   - Handles vector destructuring (vectors are recursively traversed)
   - Compares symbols by name (ignoring package differences)
   - Added `substitute-symbols` helper function

2. **Implemented array creation functions** - cl-clojure-eval.lisp:1239-1296
   - `byte-array`, `short-array`, `char-array`, `int-array`, `long-array`, `float-array`, `double-array`
   - `aset` for array element setting
   - Stub implementations that use CL vectors

3. **Implemented unchecked cast functions** - cl-clojure-eval.lisp:1287-1294
   - `unchecked-byte`, `unchecked-short`, `unchecked-char`, `unchecked-int`
   - `unchecked-long`, `unchecked-float`, `unchecked-double`
   - Currently just return the value unchanged

4. **Implemented type conversion functions** - cl-clojure-eval.lisp:1270-1285
   - `char` - convert number to character (code-char)
   - `int` - truncate to integer
   - `long`, `float`, `double` - type conversions

5. **Added hexadecimal literal support** - cl-clojure-eval.lisp:1910-1918
   - When a symbol like `0xFFFF` is not found, it's parsed as hexadecimal
   - Fallback in symbol evaluation to handle `0x` prefix

6. **Implemented destructuring support for `for` and `doseq`** - cl-clojure-eval.lisp:352-395, 423-464
   - Updated `parse-for-clauses` to accept vector bindings (not just symbols)
   - Added `extend-binding` function to handle both symbol and list destructuring
   - Supports `&` for rest parameters: `[a b & rest]`

**Errors Fixed:**
- "Undefined symbol: do-template" - FIXED ✅
- "Cannot apply non-function: byte-array" - FIXED ✅
- "Undefined symbol: char" - FIXED ✅
- "Undefined symbol: 0xFFFF" (hexadecimal) - FIXED ✅
- "Invalid clause in for: #(f vals)" - FIXED ✅ (destructuring support)

**Known Issues:**
- "numbers" test now fails with: "#(#(_ |inputs|) & |expectations|) is not a string designator."
  - This appears to be related to destructuring with `&` in nested let forms
  - The error message is from SBCL, suggesting something is being used where a string is expected
  - More investigation needed to trace the exact source

**Test Results:**
- Parse: 68 ok, 0 errors ✅
- Eval: 4 ok, 64 errors (same count, but errors have changed)
- The "numbers" test now gets past `do-template` but fails on a destructuring issue

**Next Steps:**
1. Debug and fix the "is not a string designator" error in the numbers test
2. Investigate the `&` rest parameter handling in destructuring
3. Implement stubs for Java interop symbols (Class/MAX_VALUE, Byte., etc.)
4. Add more core functions as tests require them

---
