# DEVLOG - Clojure on SBCL Development Log

## Project Status

**Goal:** Implement Clojure evaluation on SBCL. The reader parses all 68 test files.

**Current Status:**
- Reader: ✅ Complete - All 68 test files parse successfully
- Eval: ⚠️ Skeleton exists - 5/68 test files evaluate without errors
- Heap exhaustion issue fixed! Tests now run in ~9 seconds instead of crashing
- Java interop stubs implemented for common Math/System/Class methods
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

### Iteration 4 - 2025-01-17

**Focus:** Fix nested destructuring in let/loop forms

**Changes Made:**

1. **Fixed nested destructuring in `extend-binding`** - cl-clojure-eval.lisp:427-477
   - The function was only handling symbol and list binding forms
   - When destructuring nested vectors like `[[_ inputs] & expectations]`, the inner binding forms were still vectors
   - Added recursive handling: when a binding form element is a vector, convert it to a list before destructuring
   - Updated both the "has rest parameter" and "no rest parameter" branches

2. **Updated `eval-let` to use `extend-binding`** - cl-clojure-eval.lisp:315-339
   - Changed from using `env-extend-lexical` directly to using `extend-binding`
   - This allows `let` to handle vector destructuring, including nested vectors
   - Converts vector binding forms to lists before passing to `extend-binding`

3. **Updated `eval-loop` to use `extend-binding`** - cl-clojure-eval.lisp:599-624
   - Same fix as `eval-let` for the `loop` special form
   - Both `let` and `loop` now support full destructuring

**Root Cause Analysis:**
The error `#(_ inputs) is not a string designator` was misleading. The actual issue was:
- When binding form was `[[_ inputs] & expectations]`, it was converted to `'([_ inputs] & expectations)`
- The `extend-binding` function would extract `regular-bindings = '([_ inputs])`
- But the element `[_ inputs]` was still a vector (SIMPLE-VECTOR), not a list
- When `extend-binding` was called recursively with `[_ inputs]` as binding form, it didn't match `symbolp` or `listp`
- The fix: convert vector binding forms to lists before recursive destructuring

**Errors Fixed:**
- "Invalid binding form: #(_ inputs)" - FIXED ✅
- Nested destructuring now works correctly

**Test Results:**
- Parse: 68 ok, 0 errors ✅
- Eval: 4 ok, 64 errors
- The "numbers" test now gets past destructuring but fails on "Undefined symbol: Math/round"
- The "macros" test error changed from "(UNQUOTE |b|) is not a string designator" to a different error

**Next Steps:**
1. Implement stubs for Java interop symbols (Math/round, Class/TYPE, etc.)
2. Continue adding more core functions as tests require them
3. Fix remaining destructuring edge cases

---

### Iteration 5 - 2025-01-17

**Focus:** Implement Java interop symbol stubs and fix metadata handling bugs

**Changes Made:**

1. **Implemented Java interop symbol stubs** - cl-clojure-eval.lisp:842-1018
   - Added `eval-java-interop` function to handle Java class/member notation
   - Supports Math class methods (round, floor, ceil, abs, min, max, pow, sqrt, sin, cos, tan, log)
   - Supports System class methods (getProperty, getenv)
   - Supports Class class methods (forName, TYPE)
   - Supports primitive wrapper TYPE fields (Boolean/TYPE, Integer/TYPE, Long/TYPE, Float/TYPE, Double/TYPE, Character/TYPE, Byte/TYPE, Short/TYPE)
   - Supports primitive wrapper constant fields (Integer/MAX_VALUE, Integer/MIN_VALUE, etc.)
   - Supports String class methods (valueOf)
   - When symbols like `Math/round` are encountered, they return a lambda that calls `eval-java-interop`

2. **Fixed critical bug in `unwrap-value`** - cl-clojure-eval.lisp:1834-1835
   - The function was using `caddr` to get the value, but it should use `cadr`
   - The wrapped format is `(meta-wrapper value metadata)`, not `(meta-wrapper metadata value)`
   - This was causing the metadata to be returned instead of the value when unwrapping

3. **Fixed `get-wrapped-metadata`** - cl-clojure-eval.lisp:1838-1840
   - Updated to use `cddr` to get the metadata (third element)
   - Previous version was using `cadr` which was getting the value

4. **Fixed `wrap-with-meta` order** - cl-clojure-eval.lisp:1825-1827
   - Changed from `(cons metadata value)` to `(cons value metadata)`
   - Now the wrapped form is `(meta-wrapper value metadata)` which matches the unwrap functions

5. **Fixed metadata evaluation for type hints** - cl-clojure-eval.lisp:2263-2274
   - When `with-meta` is evaluated, if the metadata is a symbol (type hint), don't evaluate it
   - Type hints like `^double` are symbols that should not be evaluated as function calls
   - Only non-symbol metadata (like maps) should be evaluated

**Root Cause Analysis:**
The error `|double| is not of type REAL` was caused by multiple bugs:
1. `unwrap-value` was returning metadata instead of value due to wrong accessor (`caddr` instead of `cadr`)
2. `wrap-with-meta` was storing metadata before value, but unwrap expected value first
3. Type hints like `^double` were being evaluated, causing the `double` function to be called

**Errors Fixed:**
- "Undefined symbol: Math/round" - FIXED ✅
- "Undefined symbol: Class/TYPE" - FIXED ✅
- "Undefined symbol: Boolean/TYPE" - FIXED ✅
- "Undefined symbol: Integer/TYPE" - FIXED ✅
- "Undefined symbol: System/getProperty" - FIXED ✅
- "The value |double| is not of type REAL" - FIXED ✅ (metadata evaluation fix)
- "The value 1.2 is not of type LIST" - FIXED ✅ (unwrap-value fix)

**Test Results:**
- Parse: 68 ok, 0 errors ✅
- Eval: 5 ok, 63 errors
- The "annotations" test now passes (was failing on System/getProperty)
- The "numbers" test now gets past Math/round but fails on "Undefined symbol: defonce"

**Next Steps:**
1. Implement `defonce` special form
2. Implement `mapcat` function
3. Implement `re-find` and regex support
4. Add more Java interop methods as tests require them

---

### Iteration 6 - 2025-01-17

**Focus:** Implement defonce, fix are macro, Java constructors, thrown?, and many numeric functions

**Changes Made:**

1. **Implemented `defonce` special form** - cl-clojure-eval.lisp:240-250
   - Defines a var only if it doesn't already exist or has no value
   - Handles metadata on the name (e.g., `^:dynamic`)
   - Used for one-time initialization in test files

2. **Fixed `are` macro to evaluate argument values** - cl-clojure-eval.lisp:2054-2089
   - The arg-values were being used as-is without evaluation
   - Now evaluates each arg-value before binding to the parameter
   - Fixes tests like `(are [x y] (= x y) (+ 1.2) 1.2)` where `(+ 1.2)` needs to be evaluated

3. **Implemented Java constructor calls** - cl-clojure-eval.lisp:1062-1132
   - Symbols ending with `.` (e.g., `Byte.`, `Integer.`) are now recognized as constructors
   - Added `eval-java-constructor` function
   - Supports constructors for primitive types: `Byte.`, `Short.`, `Integer.`, `Long.`, `Float.`, `Double.`, `Character.`, `Boolean.`

4. **Implemented `thrown?` as a special form within `is`** - cl-clojure-eval.lisp:2344-2371
   - `(is (thrown? ExceptionType expr))` now catches exceptions
   - Uses `handler-case` to catch errors and return 'true on exception, nil on success
   - Added stub exception classes: `ClassCastException`, `IllegalArgumentException`, etc.

5. **Implemented arbitrary precision arithmetic functions**
   - `*'` - arbitrary precision multiplication (interned symbol)
   - `-'` - arbitrary precision subtraction (interned symbol)
   - Both are aliases to regular `*` and `-` for now

6. **Added bigint literal support** - cl-clojure-eval.lisp:2356-2365
   - Symbols ending with `N` (e.g., `123N`) are parsed as integers
   - Symbols ending with `M` (e.g., `1.2M`) are parsed as floats

7. **Added bigint/bigint conversion functions** - cl-clojure-eval.lisp:1808-1811
   - `bigint`, `biginteger`, `bigdec` - stubs that just return the number

8. **Added numeric predicate functions** - cl-clojure-eval.lisp:1215-1224, 1823-1840
   - `integer?`, `float?`, `decimal?` - type checking predicates
   - `even?`, `odd?`, `neg?`, `pos?`, `zero?` - numeric predicates
   - `true?`, `false?` - truthiness predicates

9. **Added integer division functions** - cl-clojure-eval.lisp:1244-1246, 1813-1822
   - `quot` - quotient (truncated division)
   - `rem` - remainder (same sign as dividend)
   - `mod` - modulo (same sign as divisor)

10. **Added bitwise manipulation functions** - cl-clojure-eval.lisp:1252-1262, 1841-1875
    - `bit-shift-left`, `bit-shift-right`, `unsigned-bit-shift-right`
    - `bit-and`, `bit-or`, `bit-xor`, `bit-not`
    - `bit-clear`, `bit-set`, `bit-flip`, `bit-test`

11. **Added `replicate` and `repeat` functions** - cl-clojure-eval.lisp:1277-1278, 2091-2105
    - `replicate` - creates a list with n copies of x
    - `repeat` - creates a lazy (or limited) sequence of repeated values

12. **Fixed `apply` to handle lazy ranges and vectors** - cl-clojure-eval.lisp:1575-1586
    - Converts lazy ranges to lists before appending
    - Converts vectors to lists before appending

13. **Fixed `fn` docstring handling** - cl-clojure-eval.lisp:304-323
    - Now handles `(fn name docstring [params] body)` format
    - Detects docstring by checking if first element is string and second is vector
    - Skips docstring when extracting name, params, and body

14. **Fixed `defn-` (private defn)** - cl-clojure-eval.lisp:2566-2567
    - Now properly handled as an alias to `eval-defn`

15. **Added array conversion functions** - cl-clojure-eval.lisp:1219-1226, 1703-1750
    - `bytes`, `booleans`, `shorts`, `chars`, `ints`, `longs`, `floats`, `doubles`
    - `boolean-array` function
    - Convert sequences to typed arrays (vectors for SBCL)

16. **Added type conversion functions** - cl-clojure-eval.lisp:1325-1335, 1785-1792
    - `byte`, `short` - truncate to integer
    - `class`, `type` - return type keyword

17. **Updated `is` to handle message argument** - cl-clojure-eval.lisp:2344-2357
    - `(is expr message)` format now works (message is ignored)

18. **Added `+` type checking** - cl-clojure-eval.lisp:1301-1309
    - Now throws error if any argument to `+` is not a number
    - Needed for `(is (thrown? ClassCastException (+ "ab" "cd")))` test

19. **Added static field value lookup** - cl-clojure-eval.lisp:862-878
    - Static fields like `Byte/MAX_VALUE` now return values directly instead of lambdas
    - Methods like `Math/round` still return lambdas

**Errors Fixed:**
- "Undefined symbol: defonce" - FIXED ✅
- "(+ 1.2) is not of type NUMBER" - FIXED ✅ (are macro now evaluates arg values)
- "Undefined symbol: Byte." - FIXED ✅ (Java constructor support)
- "Undefined symbol: ClassCastException" - FIXED ✅ (exception class stubs)
- "Undefined symbol: *'" - FIXED ✅ (interned symbol registration)
- "Undefined symbol: 9223372036854775808N" - FIXED ✅ (bigint literal support)
- "Undefined symbol: bigint" - FIXED ✅ (bigint conversion stub)
- "Undefined symbol: integer?" - FIXED ✅ (numeric predicates)
- "Undefined symbol: -'" - FIXED ✅ (arbitrary precision subtraction)
- "Undefined symbol: quot" - FIXED ✅ (division functions)
- "Undefined symbol: even?" - FIXED ✅ (even/odd predicates)
- "Undefined symbol: bit-shift-left" - FIXED ✅ (bit manipulation functions)
- "Undefined symbol: boolean-array" - FIXED ✅ (array functions)
- "Undefined symbol: booleans" - FIXED ✅ (array conversion functions)
- "Undefined symbol: n" (in defn- body) - FIXED ✅ (fn docstring handling)
- "#(... 1) is not of type LIST" - FIXED ✅ (apply function now handles lazy ranges)

**Test Results:**
- Parse: 68 ok, 0 errors ✅
- Eval: 5 ok, 63 errors
- The "numbers" test now gets through most of its tests but fails on "invalid number of arguments: 2"

**Known Issues:**
- "numbers" test fails with "invalid number of arguments: 2" - runtime error, source unclear
- This might be related to `str` function or some other arity issue

**Next Steps:**
1. Debug the "invalid number of arguments: 2" error in the numbers test
2. Implement `mapcat` function
3. Implement `re-find` and regex support
4. Add more core functions as tests require them
