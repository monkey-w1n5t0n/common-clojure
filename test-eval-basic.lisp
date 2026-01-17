;;;; Basic test for Clojure evaluation system

(require 'asdf)
(load "cl-clojure-syntax.asd")
(asdf:load-system :cl-clojure-syntax)
(load "cl-clojure.asd")
(asdf:load-system :cl-clojure)

;; Don't enable Clojure syntax in the test file - use explicit calls
;; (cl-clojure-syntax:enable-clojure-syntax)

;; Initialize the evaluation system
(cl-clojure-eval:init-eval-system)

;; Helper to run a test
(defun run-test (name expected actual)
  (format t "~&~a: " name)
  (format t "expected ~a, got ~a" expected actual)
  (if (equal expected actual)
      (format t " - PASS~%")
      (format t " - FAIL~%"))
  (equal expected actual))

;; Test 1: Simple arithmetic
(let ((result (cl-clojure-eval:eval-string "(+ 1 2)")))
  (run-test "Test 1: (+ 1 2)" 3 result))

;; Test 2: def
(cl-clojure-eval:eval-string "(def x 10)")

;; Test 3: symbol lookup
(let ((result (cl-clojure-eval:eval-string "x")))
  (run-test "Test 3: x (after def)" 10 result))

;; Test 4: if true
(let ((result (cl-clojure-eval:eval-string "(if true 1 2)")))
  (run-test "Test 4: (if true 1 2)" 1 result))

;; Test 5: if false
(let ((result (cl-clojure-eval:eval-string "(if false 1 2)")))
  (run-test "Test 5: (if false 1 2)" 2 result))

;; Test 6: do
(let ((result (cl-clojure-eval:eval-string "(do 1 2 3)")))
  (run-test "Test 6: (do 1 2 3)" 3 result))

;; Test 7: quote (note: Clojure reader produces lowercase symbols)
(let ((result (cl-clojure-eval:eval-string "(quote x)")))
  ;; The result is a symbol with name "x" (lowercase)
  (if (and (symbolp result) (cl:string= (symbol-name result) "x"))
      (format t "~&Test 7: (quote x): PASS~%")
      (format t "~&Test 7: (quote x): FAIL - got ~a~%" result)))

;; Test 8: let
(let ((result (cl-clojure-eval:eval-string "(let [x 5] (+ x 3))")))
  (run-test "Test 8: (let [x 5] (+ x 3))" 8 result))

;; Test 9: fn
(let ((result (cl-clojure-eval:eval-string "((fn [x] (* x 2)) 5)")))
  (run-test "Test 9: ((fn [x] (* x 2)) 5)" 10 result))

;; Test 10: defn
(cl-clojure-eval:eval-string "(defn square [x] (* x x))")
(let ((result (cl-clojure-eval:eval-string "(square 4)")))
  (run-test "Test 10: (square 4)" 16 result))

(format t "~&=== Tests Complete ===~%")
(sb-ext:quit)
