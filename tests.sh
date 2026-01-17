#!/bin/bash
# Run the Clojure test suite and exit with appropriate status.
# Exit 0 if all tests pass (all files parseable), 1 otherwise.

set -e

cd "$(dirname "$0")"

# Run the test runner and capture output
# Count errors from the output
output=$(sbcl --script test-runner.lisp 2>&1)

echo "$output"

# Extract error count from output
errors=$(echo "$output" | grep -oP 'Errors:\s*\K\d+' || echo "0")

if [ "$errors" = "0" ]; then
    exit 0
else
    exit 1
fi
