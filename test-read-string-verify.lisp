#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(defpackage #:test-read-string
  (:use #:cl #:cl-clojure-syntax #:cl-clojure-eval))

(in-package #:test-read-string)

;; Initialize
(init-eval-system)

;; Test read-string with various inputs
(format t "Testing read-string...~%")
(let ((result1 (eval-string "(read-string \"42\")")))
  (format t "(read-string \"42\"): ~A (type: ~A)~%" result1 (type-of result1)))

(let ((result2 (eval-string "(read-string \"(+ 1 2)\")")))
  (format t "(read-string \"(+ 1 2)\"): ~A (type: ~A)~%" result2 (type-of result2)))

(let ((result3 (eval-string "(read-string \"\\\"hello\\\"\")")))
  (format t "(read-string \"\\\"hello\\\"\"): ~A (type: ~A)~%" result3 (type-of result3)))
