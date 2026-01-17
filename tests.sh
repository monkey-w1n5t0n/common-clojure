#!/bin/bash
# Run the Clojure test suite and exit with appropriate status.
# Exit 0 only if all tests PASS, exit 1 otherwise.
#
# The test-runner.lisp handles exit codes:
# - 0: All tests passed
# - 1: Tests failed or evaluation not yet implemented

cd "$(dirname "$0")"

# Run the test runner - it handles its own exit code
exec sbcl --script test-runner.lisp
