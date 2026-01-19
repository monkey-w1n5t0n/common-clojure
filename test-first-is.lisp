#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Setup ns
(clojure-eval '(ns clojure.test-clojure.array-symbols
                 (use clojure.test)
                 (require #(clojure.test-helper as util))) *current-env*)

;; Now test the first is
(format t "Testing first is...~%")
(let ((is-form '(is (= 'java.lang.String/1 (read-string "java.lang.String/1")))))
  (handler-case
      (let ((result (clojure-eval is-form *current-env*)))
        (format t "Result: ~A~%" result))
    (error (c)
      (format t "ERROR: ~A~%" c)
      (format t "Type: ~A~%" (type-of c))
      (sb-debug:backtrace 10))))
