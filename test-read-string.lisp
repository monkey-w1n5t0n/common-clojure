#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(defpackage #:test-read-string
  (:use #:cl #:cl-clojure-syntax #:cl-clojure-eval))

(in-package #:test-read-string)

;; Initialize
(init-eval-system)

;; Test read-string
(handler-case
    (progn
      (format t "Testing read-string...~%")
      (let ((result (eval-string "(read-string \"42\")")))
        (format t "Result: ~A~%" result)))
  (error (c)
    (format t "Error: ~A~%" c)))
