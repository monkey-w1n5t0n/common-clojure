#!/bin/bash
cd /home/w1n5t0n/src/common-clojure

OUTPUT="/tmp/clojure-error-analysis.txt"

TESTS="agents array_symbols data_structures errors for metadata multimethods parse printer protocols reducers test test_let_bindings transducers vectors other_functions sequences special compilation clojure_walk clearing evaluation"

echo "Analyzing 22 test files (detailed)..." >&2

rm -f $OUTPUT

for test in $TESTS; do
    echo "  $test..." >&2
    cat > /tmp/diag-test.lisp << EOF
(progn
  (handler-case
    (cl-clojure-eval::eval-file "clojure-tests/${test}.clj")
    (error (c)
      (let* ((msg (format nil "~A" c))
             (first-nl (position #\Newline msg)))
        (if first-nl
          (format t "FIRST-ERROR: ~A~%" (subseq msg 0 first-nl))
          (format t "FIRST-ERROR: ~A~%" msg)))))
  (sb-ext:exit :code 0))
EOF

    RESULT=$(sbcl --noinform --disable-debugger --non-interactive \
      --load sbcl-init.lisp \
      --load package.lisp \
      --load cl-clojure-syntax.lisp \
      --eval '(let ((*readtable* (copy-readtable nil))) (load "cl-clojure-eval.lisp") (load "cl-clojure-transducers.lisp"))' \
      --load /tmp/diag-test.lisp \
      2>/dev/null)

    # Get the FIRST-ERROR line and take first 300 chars
    ERROR=$(echo "$RESULT" | grep "^FIRST-ERROR:" | head -1 | sed 's/^FIRST-ERROR: //' | head -c 300)

    if [ -z "$ERROR" ]; then
        echo "=== $test === PASSED" >> $OUTPUT
    else
        echo "=== $test === FAILED" >> $OUTPUT
        echo "  First Error: $ERROR" >> $OUTPUT
        echo "" >> $OUTPUT
    fi
done

echo "" >&2
cat $OUTPUT
