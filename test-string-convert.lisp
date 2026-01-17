#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")

(defpackage #:test-string-convert
  (:use #:cl #:cl-clojure-syntax))

(in-package #:test-string-convert)

(let ((*readtable* (ensure-clojure-readtable)))
  (let ((form (read-from-string "\"42\"")))
    (format t "Form: ~A~%" form)
    (format t "Type: ~A~%" (type-of form))
    (format t "Convert result: ~A~%" (convert-cl-quasiquote form))
    (format t "Convert type: ~A~%" (type-of (convert-cl-quasiquote form)))))

;; Test with simple form
(let ((*readtable* (ensure-clojure-readtable)))
  (let ((form (read-from-string "(+ 1 2)")))
    (format t "~%Form: ~A~%" form)
    (format t "Convert result: ~A~%" (convert-cl-quasiquote form))))
