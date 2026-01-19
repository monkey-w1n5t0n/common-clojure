#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

(defpackage #:clojure-test-runner
  (:use #:cl #:cl-clojure-syntax #:cl-clojure-eval))

(in-package #:clojure-test-runner)

(handler-case
    (progn
      (unless *current-env*
        (init-eval-system))
      ;; Try to evaluate a simple complement function call
      (let ((code "(complement contains?)"))
        (format t "Evaluating: ~A~%" code)
        (let ((result (clojure-eval (read-from-string code))))
          (format t "Result: ~A~%" result))))
  (error (c)
    (format t "Error: ~A~%" c)
    (format t "Backtrace: ~A~%" (sb-debug:backtrace-as-list 10))))
