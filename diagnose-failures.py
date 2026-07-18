#!/usr/bin/env python3
"""Diagnose failing Clojure test files by running each one individually in SBCL."""

import subprocess
import os
import re

BASEDIR = "/home/w1n5t0n/src/common-clojure"

FAILING_TESTS = [
    "agents", "array_symbols", "clearing", "clojure_walk", "clojure_xml",
    "compilation", "data_structures", "errors", "evaluation", "for",
    "genclass", "java_interop", "main", "metadata", "method_thunks",
    "multimethods", "other_functions", "param_tags", "parse", "predicates",
    "printer", "protocols", "reducers", "reflect", "rt", "sequences",
    "serialization", "special", "streams", "string", "test",
    "test_let_bindings", "transducers", "vectors"
]

def generate_lisp_code(test_path):
    """Generate Lisp diagnostic code for a single test file."""
    # Write each line carefully to ensure correct paren nesting
    lines = []
    lines.append('(load "sbcl-init.lisp")')
    lines.append('(load "package.lisp")')
    lines.append('(load "cl-clojure-syntax.lisp")')
    lines.append('(load "cl-clojure-eval.lisp")')
    lines.append('(load "cl-clojure-transducers.lisp")')
    lines.append('(setf *readtable* (copy-readtable nil))')
    lines.append('(in-package :cl-clojure-eval)')
    lines.append('(setf *current-env* nil)')
    lines.append('(init-eval-system)')
    lines.append('(handler-case')
    lines.append('  (let* ((path #p"' + test_path + '")')
    lines.append('         (content (with-open-file (s path :direction :input)')
    lines.append('                    (let ((str (make-string (file-length s))))')
    lines.append('                      (read-sequence str s) str)))')
    lines.append('         (preprocessed (cl-clojure-syntax:preprocess-clojure-dots content))')
    lines.append('         (comment-marker (cl-clojure-syntax:get-comment-marker))')
    lines.append('         (form-count 0))')
    lines.append('    (with-input-from-string (stream preprocessed)')
    lines.append('      (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))')
    lines.append('        (loop for form = (cl-clojure-syntax:read-clojure stream nil :eof)')
    lines.append('              until (eq form :eof)')
    lines.append('              do (unless (eq form comment-marker)')
    lines.append('                   (incf form-count)')
    lines.append('                   (handler-case')
    lines.append('                     (clojure-eval form *current-env*)')
    lines.append('                     (error (c)')
    lines.append('                       (format t "EVAL-ERROR at form #~D: ~A~%" form-count c)')
    lines.append('                       (sb-ext:exit :code 0)))))))')
    lines.append('    (format t "PASSED~%"))')
    lines.append('  (cl:reader-error (c) (format t "READER-ERROR: ~A~%" c))')
    lines.append('  (cl:stream-error (c) (format t "STREAM-ERROR: ~A~%" c))')
    lines.append('  (cl:end-of-file (c) (format t "EOF-ERROR: ~A~%" c))')
    lines.append('  (cl:type-error (c) (format t "TYPE-ERROR: ~A~%" c))')
    lines.append('  (cl:error (c) (format t "ERROR: ~A~%" c)))')
    lines.append('(sb-ext:exit :code 0)')
    return '\n'.join(lines)

def diagnose_test(test_name):
    """Run a single test file and capture the first error."""
    test_path = os.path.join(BASEDIR, "clojure-tests", f"{test_name}.clj")
    lisp_code = generate_lisp_code(test_path)

    tmp_file = f"/tmp/diag_{test_name}.lisp"
    with open(tmp_file, 'w') as f:
        f.write(lisp_code)

    try:
        result = subprocess.run(
            ['sbcl', '--noinform', '--disable-debugger', '--non-interactive',
             '--load', tmp_file],
            capture_output=True, text=True, timeout=60,
            cwd=BASEDIR
        )
        stdout = result.stdout.strip()
        # Filter stderr to get just error lines
        stderr_lines = [l for l in result.stderr.split('\n')
                        if l.strip() and not l.strip().startswith(';')
                        and not l.strip().startswith('WARNING')
                        and 'compilation unit' not in l]
        stderr = '\n'.join(stderr_lines).strip()

        if stdout:
            return stdout
        elif 'unhandled condition' in stderr.lower() or 'Unhandled' in stderr:
            # Extract the actual error from stderr
            for line in stderr.split('\n'):
                if 'Unhandled' in line or 'ERROR' in line:
                    return f"LISP-CRASH: {line.strip()}"
            return f"LISP-CRASH: {stderr[:200]}"
        return f"(no output, exit={result.returncode})"
    except subprocess.TimeoutExpired:
        return "TIMEOUT after 60s"
    except Exception as e:
        return f"EXCEPTION: {e}"

def categorize_failure(output):
    """Categorize the failure based on the error output."""
    output_lower = output.lower()

    if "reader-error" in output_lower or "eof-error" in output_lower:
        return "missing-reader-features"
    if "stream-error" in output_lower and "end of file" in output_lower:
        return "missing-reader-features"
    if "unknown special form" in output_lower:
        return "missing-special-forms"
    if "undefined symbol" in output_lower:
        return "missing-core-functions"
    if "unknown function" in output_lower or "cannot apply non-function" in output_lower:
        return "missing-core-functions"
    if "unsupported java" in output_lower or "unsupported uuid" in output_lower:
        return "java-interop"
    if "java" in output_lower and ("constructor" in output_lower or "interop" in output_lower or "method" in output_lower):
        return "java-interop"
    if "unsupported" in output_lower and "method" in output_lower:
        return "java-interop"
    if "stack overflow" in output_lower or "control stack" in output_lower:
        return "stack-overflow"
    if "lisp-crash" in output_lower:
        return "lisp-crash"
    if "timeout" in output_lower:
        return "timeout"
    return "eval-errors"

def extract_missing_feature(output):
    """Try to extract the specific missing feature from the error."""
    # Unknown special form
    m = re.search(r'Unknown special form[:\s]*(\S+)', output, re.IGNORECASE)
    if m:
        return f"Missing special form: {m.group(1)}"

    # Unknown function
    m = re.search(r'Unknown function[:\s]*(\S+)', output, re.IGNORECASE)
    if m:
        return f"Missing function: {m.group(1)}"

    # Cannot apply non-function
    m = re.search(r'Cannot apply non-function[:\s]*(\S+)', output, re.IGNORECASE)
    if m:
        return f"Missing function: {m.group(1)}"

    # Try to get the actual error from EVAL-ERROR output
    m = re.search(r'EVAL-ERROR at form #(\d+): (.*)', output, re.DOTALL)
    if m:
        error_msg = m.group(2).strip()
        if len(error_msg) > 300:
            error_msg = error_msg[:297] + "..."
        return f"Form #{m.group(1)}: {error_msg}"

    # READER-ERROR
    m = re.search(r'READER-ERROR: (.*)', output, re.DOTALL)
    if m:
        error_msg = m.group(1).strip()
        if len(error_msg) > 300:
            error_msg = error_msg[:297] + "..."
        return f"Reader error: {error_msg}"

    # EOF-ERROR
    m = re.search(r'EOF-ERROR: (.*)', output, re.DOTALL)
    if m:
        return f"EOF error: {m.group(1)[:200]}"

    # STREAM-ERROR
    m = re.search(r'STREAM-ERROR: (.*)', output, re.DOTALL)
    if m:
        return f"Stream error: {m.group(1)[:200]}"

    # ERROR
    m = re.search(r'^ERROR: (.*)', output, re.MULTILINE)
    if m:
        return f"Error: {m.group(1)[:200]}"

    # LISP-CRASH
    m = re.search(r'LISP-CRASH: (.*)', output)
    if m:
        return m.group(1)[:120]

    return output[:120] if len(output) > 120 else output

if __name__ == "__main__":
    print(f"=== Diagnosing {len(FAILING_TESTS)} Failing Test Files ===\n")

    results = []
    for i, test_name in enumerate(FAILING_TESTS):
        print(f"[{i+1}/{len(FAILING_TESTS)}] {test_name}...", end=" ", flush=True)
        output = diagnose_test(test_name)
        category = categorize_failure(output)
        feature = extract_missing_feature(output)
        results.append((test_name, output, category, feature))
        print(f"[{category}]")

    # Print detailed results
    print("\n\n=== DETAILED RESULTS ===")
    for test_name, output, category, feature in results:
        print(f"\n--- {test_name} ---")
        print(f"  Category: {category}")
        print(f"  First Error: {feature}")
        if len(output) > 200:
            print(f"  Full Output (first 500 chars): {output[:500]}")

    # Print summary table
    print("\n\n=== SUMMARY TABLE ===")
    print(f"{'Test File':<40} | {'Category':<25} | {'Missing Feature / First Error'}")
    print("-" * 120)
    for test_name, output, category, feature in results:
        print(f"{test_name:<40} | {category:<25} | {feature}")

    # Group by category
    print("\n\n=== GROUPED BY CATEGORY ===")
    categories = {}
    for test_name, output, category, feature in results:
        if category not in categories:
            categories[category] = []
        categories[category].append((test_name, feature))

    for cat, tests in sorted(categories.items()):
        print(f"\n--- {cat} ({len(tests)} tests) ---")
        for test_name, feature in tests:
            print(f"  {test_name:<35} | {feature}")
