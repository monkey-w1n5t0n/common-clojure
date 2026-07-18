#!/bin/bash
cd /home/w1n5t0n/src/common-clojure

OUTPUT="/tmp/clojure-error-analysis.txt"

TESTS="agents array_symbols data_structures errors for metadata multimethods parse printer protocols reducers test test_let_bindings transducers vectors other_functions sequences special compilation clojure_walk clearing evaluation"

echo "Analyzing 22 test files (full errors)..." >&2

rm -f $OUTPUT

for test in $TESTS; do
    echo "  $test..." >&2
    # Capture more of the error by printing first 2 lines
    cat > /tmp/diag-test.lisp << EOF
(progn
  (handler-case
    (cl-clojure-eval::eval-file "clojure-tests/${test}.clj")
    (error (c)
      (format t "FIRST-ERROR: ~A~%" c)))
  (sb-ext:exit :code 0))
EOF

    RESULT=$(sbcl --noinform --disable-debugger --non-interactive \
      --load sbcl-init.lisp \
      --load package.lisp \
      --load cl-clojure-syntax.lisp \
      --eval '(let ((*readtable* (copy-readtable nil))) (load "cl-clojure-eval.lisp") (load "cl-clojure-transducers.lisp"))' \
      --load /tmp/diag-test.lisp \
      2>/dev/null)

    # Get FIRST-ERROR and take up to 500 chars (may span lines)
    ERROR=$(echo "$RESULT" | sed -n '/^FIRST-ERROR: /{s/^FIRST-ERROR: //;p;q}' | head -c 500)

    if [ -z "$ERROR" ]; then
        echo "=== $test === PASSED" >> $OUTPUT
    else
        # Take first line only for table, but keep full for detail
        ERROR_LINE1=$(echo "$ERROR" | head -1)
        echo "=== $test === FAILED" >> $OUTPUT
        echo "  First Error: $ERROR_LINE1" >> $OUTPUT
        echo "" >> $OUTPUT
    fi
done

cat $OUTPUT
