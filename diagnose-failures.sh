#!/bin/bash
# Diagnose failing Clojure test files one by one
# Each test gets its own SBCL invocation

cd /home/w1n5t0n/src/common-clojure

TESTS="agents array_symbols clearing clojure_walk clojure_xml compilation data_structures errors evaluation for genclass java_interop main metadata method_thunks multimethods other_functions param_tags parse predicates printer protocols reducers reflect rt sequences serialization special streams string test test_let_bindings transducers vectors"

BASEDIR="/home/w1n5t0n/src/common-clojure"

echo "=== Diagnosing $(echo $TESTS | wc -w) Failing Test Files ==="
echo ""

for test_name in $TESTS; do
    echo "--- $test_name ---"
    sbcl --noinform --disable-debugger --non-interactive \
      --eval '(load "sbcl-init.lisp")' \
      --eval '(load "package.lisp")' \
      --eval '(load "cl-clojure-syntax.lisp")' \
      --eval '(load "cl-clojure-eval.lisp")' \
      --eval '(load "cl-clojure-transducers.lisp")' \
      --eval '(setf *readtable* (copy-readtable nil))' \
      --eval '(in-package :cl-clojure-eval)' \
      --eval '(setf *current-env* nil)' \
      --eval '(init-eval-system)' \
      --eval "(handler-case
        (let* ((path #p\"$BASEDIR/clojure-tests/$test_name.clj\")
               (content (with-open-file (s path :direction :input)
                          (let ((str (make-string (file-length s))))
                            (read-sequence str s) str)))
               (preprocessed (cl-clojure-syntax:preprocess-clojure-dots content))
               (comment-marker (cl-clojure-syntax:get-comment-marker))
               (form-count 0))
          (with-input-from-string (stream preprocessed)
            (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
              (loop for form = (cl-clojure-syntax:read-clojure stream nil :eof)
                    until (eq form :eof)
                    do (unless (eq form comment-marker)
                         (incf form-count)
                         (handler-case
                           (clojure-eval form *current-env*)
                           (error (c)
                             (format t \"EVAL-ERROR at form #~D: ~A~%\" form-count c)
                             (return))))))))
        (format t \"PASSED~%\"))
      (cl:reader-error (c) (format t \"READER-ERROR: ~A~%\" c))
      (cl:stream-error (c) (format t \"STREAM-ERROR: ~A~%\" c))
      (cl:end-of-file (c) (format t \"EOF-ERROR: ~A~%\" c))
      (cl:type-error (c) (format t \"TYPE-ERROR: ~A~%\" c))
      (cl:error (c) (format t \"ERROR: ~A~%\" c))" \
      2>/dev/null
    echo ""
done

echo "Done."
