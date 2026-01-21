#!/bin/bash
# Run the Clojure test suite and exit with appropriate status.
# Exit 0 only if all tests PASS, exit 1 otherwise.

cd "$(dirname "$0")"

# Run the test runner - using run-tests.lisp
# Load without script mode to avoid shebang issues
exec sbcl --noinform --load run-tests.lisp --quit
