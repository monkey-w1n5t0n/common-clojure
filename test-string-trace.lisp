#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")

(defpackage #:test-string-trace
  (:use #:cl #:cl-clojure-syntax))

(in-package #:test-string-trace)

;; Test preprocessing
(let* ((input "\"42\"")
       (preprocessed (preprocess-clojure-dots input)))
  (format t "Input: ~A~%" input)
  (format t "Preprocessed: ~A~%" preprocessed)
  (format t "Preprocessed length: ~D~%" (length preprocessed))
  (format t "Char codes: ")
  (loop for c across preprocessed
        do (format t "~D " (char-code c)))
  (terpri))
