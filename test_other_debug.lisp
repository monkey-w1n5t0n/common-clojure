#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

(defpackage #:clojure-test-runner
  (:use #:cl #:cl-clojure-syntax #:cl-clojure-eval))

(in-package #:clojure-test-runner)

(defun test-file (path)
  (handler-case
      (progn
        (unless *current-env*
          (init-eval-system))
        (eval-file (namestring path))
        (format t "~&=== Test OK: ~A ===~%" (pathname-name path)))
    (error (c)
      (format t "~&=== Test FAILED: ~A ===~%" (pathname-name path))
      (format t "Error: ~A~%" c))))

(test-file "clojure-tests/other_functions.clj")
