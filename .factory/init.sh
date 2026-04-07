#!/bin/bash
# Initialize environment for Clojure-on-Common-Lisp development
set -e

# Ensure SBCL is available
if ! command -v sbcl &> /dev/null; then
    echo "ERROR: SBCL (Steel Bank Common Lisp) is required but not found"
    exit 1
fi

# Check that required files exist
if [ ! -f "cl-clojure-eval.lisp" ]; then
    echo "ERROR: cl-clojure-eval.lisp not found - are you in the right directory?"
    exit 1
fi

# Quick smoke test - can SBCL load the system?
sbcl --noinform --non-interactive \
  --load sbcl-init.lisp --load package.lisp \
  --load cl-clojure-syntax.lisp \
  --eval '(let ((*readtable* (copy-readtable nil))) (load "cl-clojure-eval.lisp") (load "cl-clojure-transducers.lisp"))' \
  --eval '(progn (cl-clojure-eval::init-eval-system) (format t "OK: System loaded~%") (sb-ext:exit :code 0))' \
  2>&1 | tail -5

echo "Environment ready. Run 'sbcl --script run-tests.lisp' to execute test suite."
