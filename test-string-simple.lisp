#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")

(defpackage #:test-string-simple
  (:use #:cl))

(in-package #:test-string-simple)

;; Use the fully qualified function
(let ((result (cl-clojure-syntax:read-clojure-string "\"42\"")))
  (format t "Result: ~A~%" result)
  (format t "Type: ~A~%" (type-of result)))
