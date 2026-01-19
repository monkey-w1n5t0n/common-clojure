#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Test ns
(clojure-eval '(ns clojure.test-clojure.array-symbols
                 (use clojure.test)
                 (require #(clojure.test-helper as util))) *current-env*)

;; Test the first 'is' form
(format t "Testing: (is (= 'java.lang.String/1 (read-string \"java.lang.String/1\")))~%")
(let ((form '(is (= 'java.lang.String/1 (read-string "java.lang.String/1")))))
  (format t "Form: ~A~%" form)
  (format t "Length: ~A~%" (length form))
  (handler-case
      (let ((result (clojure-eval form *current-env*)))
        (format t "Result: ~A~%" result))
    (error (c)
      (format t "ERROR: ~A~%" c)
      (format t "Type: ~A~%" (type-of c))
      (sb-debug:backtrace))))
