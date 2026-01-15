(require 'asdf)
(load "cl-clojure-syntax.asd")
(asdf:load-system :cl-clojure-syntax)

(defpackage #:cl-clojure-syntax-tests
  (:use #:cl #:cl-clojure-syntax))

(in-package #:cl-clojure-syntax-tests)

(defvar *test-failures* 0)

(defmacro assert-equal (expected actual &optional message)
  `(let ((exp ,expected)
         (act ,actual))
     (if (equalp exp act)
         (format t "PASS: ~A~%" (or ,message (format nil "~s == ~s" ',expected ',actual)))
         (progn
           (format t "FAIL: ~A~%  Expected: ~S~%  Actual:   ~S~%" 
                   (or ,message "Assertion failed") exp act)
           (incf *test-failures*)))))

(defun run-tests ()
  (format t "Running tests...~%")
  (enable-clojure-syntax)
  
  ;; Test Vectors
  (format t "~%Testing Vectors...~%")
  (assert-equal #(1 2 3) (read-from-string "[1 2 3]") "Simple vector")
  (assert-equal #() (read-from-string "[]") "Empty vector")
  (assert-equal #(1 "two" :three) (read-from-string "[1 \"two\" :three]") "Mixed types vector")
  
  ;; Test Maps
  (format t "~%Testing Maps...~%")
  (let ((h (read-from-string "{:a 1 :b 2}")))
    (assert-equal 'hash-table (type-of h) "Is a hash table")
    (assert-equal 2 (hash-table-count h) "Correct count")
    (assert-equal 1 (gethash :a h) "Key :a is 1")
    (assert-equal 2 (gethash :b h) "Key :b is 2"))
  
  (let ((h (read-from-string "{}")))
    (assert-equal 0 (hash-table-count h) "Empty map"))

  ;; Test Nested
  (format t "~%Testing Nested...~%")
  (let ((nested (read-from-string "[{:a 1} 2]")))
    (assert-equal 1 (gethash :a (aref nested 0)) "Nested map in vector"))

  ;; Test read-time evaluation prevention (basic check)
  ;; Clojure syntax is read-time, so it produces data structures.

  ;; Test Deep Nesting
  (format t "~%Testing Deep Nesting...~%")
  (assert-equal #(#(#(1))) (read-from-string "[[[1]]]") "Deep nested vectors")
  (assert-equal #((a b)) (read-from-string "[[a b]]") "Deep nested with symbols")
  (let ((deep-map (read-from-string "{:a {:b {:c 1}}}")))
    (assert-equal 1 (gethash :c (gethash :b (gethash :a deep-map))) "Deep nested maps"))

  ;; Test Complex Keys in Maps
  (format t "~%Testing Complex Map Keys...~%")
  (let ((h (read-from-string "{[:a :b] 1 {:c 2} 3}")))
    (assert-equal 2 (hash-table-count h) "Complex keys count")
    ;; Vector key lookup requires constructing the same vector
    (assert-equal 1 (gethash #(:a :b) h) "Vector key lookup"))
  (let ((h (read-from-string "{{:inner 1} :outer}")))
    (assert-equal :outer (gethash #(:inner 1) h) "Map as key"))

  ;; Test Vector with Expressions
  (format t "~%Testing Vectors with Expressions...~%")
  (assert-equal #(6) (read-from-string "[(* 2 3)]") "Vector with expression")
  (assert-equal #(1 4 9) (read-from-string "[(* 1 1) (* 2 2) (* 3 3)]") "Vector with multiple expressions")

  ;; Test Larger Structures
  (format t "~%Testing Larger Structures...~%")
  (let ((big-vec (read-from-string "[1 2 3 4 5 6 7 8 9 10]")))
    (assert-equal 10 (length big-vec) "10 element vector"))
  (let ((big-map (read-from-string "{:a 1 :b 2 :c 3 :d 4 :e 5}")))
    (assert-equal 5 (hash-table-count big-map) "5 key map"))

  ;; Test Error Conditions (Odd number of map elements)
  (format t "~%Testing Error Conditions...~%")

  ;; Odd number of map elements
  (handler-case
      (progn
        (read-from-string "{:a 1 :b}")
        (format t "FAIL: Should have signaled error for odd map elements~%")
        (incf *test-failures*))
    (error (c)
      (format t "PASS: Caught expected error (odd map elements): ~A~%" c)))

  ;; Unmatched closing delimiter ]
  (handler-case
      (progn
        (read-from-string "]")
        (format t "FAIL: Should have signaled error for unmatched ]~%")
        (incf *test-failures*))
    (error (c)
      (format t "PASS: Caught expected error (unmatched ]): ~A~%" c)))

  ;; Unmatched closing delimiter }
  (handler-case
      (progn
        (read-from-string "}")
        (format t "FAIL: Should have signaled error for unmatched }~%")
        (incf *test-failures*))
    (error (c)
      (format t "PASS: Caught expected error (unmatched }): ~A~%" c)))

  ;; Test disable/enable round-trip
  (format t "~%Testing Enable/Disable Round-Trip...~%")
  (disable-clojure-syntax)
  (handler-case
      (progn
        (read-from-string "[1 2 3]")
        (format t "FAIL: Should have signaled error when syntax disabled~%")
        (incf *test-failures*))
    (error (c)
      (format t "PASS: Caught expected error (syntax disabled): ~A~%" c)))
  ;; Re-enable for final checks
  (enable-clojure-syntax)
  (assert-equal #(1 2 3) (read-from-string "[1 2 3]") "Re-enable syntax works")

  (if (= 0 *test-failures*)
      (format t "~%ALL TESTS PASSED!~%")
      (format t "~%FAILED: ~D tests.~%" *test-failures*))
  
  (sb-ext:exit :code (if (= 0 *test-failures*) 0 1)))

(run-tests)
