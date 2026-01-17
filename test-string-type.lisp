#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(defpackage #:test-string-type
  (:use #:cl #:cl-clojure-syntax #:cl-clojure-eval))

(in-package #:test-string-type)

;; Test string reading
(let ((*readtable* (ensure-clojure-readtable)))
  (let ((result (read-from-string "\"42\"")))
    (format t "read-from-string \"\\\"42\\\"\": ~A~%" result)
    (format t "class-of: ~A~%" (class-of result)))
  (let ((result (cl-clojure-syntax:read-clojure-string "\"42\"")))
    (format t "read-clojure-string \"\\\"42\\\"\": ~A~%" result)
    (format t "class-of: ~A~%" (class-of result))))
