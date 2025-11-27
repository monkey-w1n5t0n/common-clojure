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
  
  ;; Test Error Condition (Odd number of map elements)
  (format t "~%Testing Error Conditions...~%")
  (handler-case 
      (progn
        (read-from-string "{:a 1 :b}")
        (format t "FAIL: Should have signaled error for odd map elements~%")
        (incf *test-failures*))
    (error (c)
      (format t "PASS: Caught expected error: ~A~%" c)))

  (if (= 0 *test-failures*)
      (format t "~%ALL TESTS PASSED!~%")
      (format t "~%FAILED: ~D tests.~%" *test-failures*))
  
  (sb-ext:exit :code (if (= 0 *test-failures*) 0 1)))

(run-tests)
