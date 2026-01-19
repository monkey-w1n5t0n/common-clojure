#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Try evaluating some forms from array_symbols.clj
(format t "Testing array_symbols forms...~%")

;; Test 1: Simple symbol resolution
(handler-case
    (progn
      (format t "Test 1: (resolve 'boolean/1) = ~A~%" (clojure-eval '(resolve 'boolean/1) *current-env*))
      (format t "PASSED~%~%"))
  (error (c)
    (format t "FAILED: ~A~%~%" c)))

;; Test 2: Read array symbol
(handler-case
    (progn
      (let* ((test-str "(= 'java.lang.String/1 (read-string \"java.lang.String/1\"))"))
        (format t "Test 2: ~A~%" test-str)
        (with-input-from-string (stream (cl-clojure-syntax:preprocess-clojure-dots test-str))
          (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
            (let ((form (cl-clojure-syntax:read-clojure stream)))
              (format t "  Form: ~A~%" form)
              (let ((result (clojure-eval form *current-env*)))
                (format t "  Result: ~A~%" result)
                (format t "PASSED~%~%")))))))
  (error (c)
    (format t "FAILED: ~A~%~%" c)))

;; Test 3: More complex - Class/forName
(handler-case
    (progn
      (format t "Test 3: (Class/forName \"[Z\")~%")
      (let ((result (clojure-eval '(Class/forName "[Z") *current-env*)))
        (format t "  Result: ~A~%" result)
        (format t "PASSED~%~%")))
  (error (c)
    (format t "FAILED: ~A~%~%" c)))

;; Test 4: Full form from test
(handler-case
    (progn
      (format t "Test 4: (= (Class/forName \"[Z\") (resolve 'boolean/1))~%")
      (let ((result (clojure-eval '(= (Class/forName "[Z") (resolve 'boolean/1)) *current-env*)))
        (format t "  Result: ~A~%" result)
        (format t "PASSED~%~%")))
  (error (c)
    (format t "FAILED: ~A~%~%" c)))

;; Test 5: are macro
(handler-case
    (progn
      (format t "Test 5: (are [str-repr klass] (= (Class/forName str-repr) klass) \"[Z\" (resolve 'boolean/1))~%")
      (let ((result (clojure-eval '(are [str-repr klass] (= (Class/forName str-repr) klass) "[Z" (resolve 'boolean/1)) *current-env*)))
        (format t "  Result: ~A~%" result)
        (format t "PASSED~%~%")))
  (error (c)
    (format t "FAILED: ~A~%~%" c)))

;; Test 6: Simple ns declaration
(handler-case
    (progn
      (format t "Test 6: (ns clojure.test-clojure.array-symbols (:use clojure.test) (:require [clojure.test-helper :as util]))~%")
      (clojure-eval '(ns clojure.test-clojure.array-symbols (:use clojure.test) (:require [clojure.test-helper :as util])) *current-env*)
      (format t "PASSED~%~%"))
  (error (c)
    (format t "FAILED: ~A~%~%" c)))

;; Test 7: Set! warn-on-reflection
(handler-case
    (progn
      (format t "Test 7: (set! *warn-on-reflection* true)~%")
      (let ((result (clojure-eval '(set! *warn-on-reflection* true) *current-env*)))
        (format t "  Result: ~A~%" result)
        (format t "PASSED~%~%")))
  (error (c)
    (format t "FAILED: ~A~%~%" c)))
