#!/usr/bin/sbcl --script

(require 'asdf)
(load "cl-clojure-syntax.asd")
(asdf:load-system :cl-clojure-syntax)

;; Test anonymous function literal reader
;; We need to read the Clojure forms first, then process them

;; Read all test forms before enabling Clojure syntax in the current package
(defun run-tests ()
  ;; Enable Clojure syntax
  (cl-clojure-syntax:enable-clojure-syntax)

  (format t "=== Testing Anonymous Function Literal Reader ===~%")

  ;; Test 1: Simple anon function with % and %2
  (let ((result (read-from-string "#(+ % %2)")))
    (format t "#(+ % %2) => ~a~%" result))

  ;; Test 2: Simple identity
  (let ((result (read-from-string "#(identity %)")))
    (format t "#(identity %) => ~a~%" result))

  ;; Test 3: Explicit %1 and %2
  (let ((result (read-from-string "#(%1 %2)")))
    (format t "#(%1 %2) => ~a~%" result))

  ;; Test 4: No arguments
  (let ((result (read-from-string "#(42)")))
    (format t "#(42) => ~a~%" result))

  ;; Test 5: Single argument with %
  (let ((result (read-from-string "#(* % 2)")))
    (format t "#(* % 2) => ~a~%" result))

  ;; Test 6: Multiple arguments %1 %2 %3
  (let ((result (read-from-string "#(+ %1 %2 %3)")))
    (format t "#(+ %1 %2 %3) => ~a~%" result))

  ;; Test 7: Rest arguments with %&
  (let ((result (read-from-string "#(vector %& )")))
    (format t "#(vector %&) => ~a~%" result))

  ;; Test 8: Nested list with placeholders
  (let ((result (read-from-string "#(map (* % 2) %&)")))
    (format t "#(map (* % 2) %&) => ~a~%" result))

  ;; Test 9: Vector with placeholders
  (let ((result (read-from-string "#([%1 %2 %3])")))
    (format t "#([%1 %2 %3]) => ~a~%" result))

  ;; Test 10: Error case - %%
  (handler-case
      (let ((result (read-from-string "#(%%)")))
        (format t "ERROR: Should have failed for %%~%"))
    (error (c)
      (format t "Correctly rejected %%: ~a~%" c)))

  (format t "~%All tests completed successfully!~%"))

;; Run the tests
(run-tests)
