#!/bin/bash
cd /home/w1n5t0n/src/common-clojure

OUTPUT="/tmp/clojure-error-analysis.txt"
echo "DETAILED ERROR ANALYSIS FOR 22 NON-JAVA-INTEROP TEST FILES" > $OUTPUT
echo "" >> $OUTPUT

TESTS="agents array_symbols data_structures errors for metadata multimethods parse printer protocols reducers test test_let_bindings transducers vectors other_functions sequences special compilation clojure_walk clearing evaluation"

for test in $TESTS; do
    echo "  $test..." >&2
    RESULT=$(sbcl --noinform --disable-debugger --non-interactive \
      --load sbcl-init.lisp \
      --load package.lisp \
      --load cl-clojure-syntax.lisp \
      --eval '(let ((*readtable* (copy-readtable nil))) (load "cl-clojure-eval.lisp") (load "cl-clojure-transducers.lisp"))' \
      --eval "(progn
        (handler-case
          (cl-clojure-eval::eval-file \"clojure-tests/${test}.clj\")
          (error (c) (format t \"ERROR: ~A\" c)))
        (sb-ext:exit :code 0))" 2>/dev/null)

    ERROR=$(echo "$RESULT" | grep "^ERROR:" | head -1 | sed 's/^ERROR: //')

    if [ -z "$ERROR" ]; then
        echo "=== $test === PASSED" >> $OUTPUT
    else
        echo "=== $test ===" >> $OUTPUT
        echo "  First Error: $ERROR" >> $OUTPUT
        echo "" >> $OUTPUT
    fi
done

echo "" >> $OUTPUT
echo "=== SUMMARY ===" >> $OUTPUT
echo "" >> $OUTPUT
cat $OUTPUT
