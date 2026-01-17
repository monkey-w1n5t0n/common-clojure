#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")

(defpackage #:test-string-debug
  (:use #:cl))

(in-package #:test-string-debug)

;; Test read-clojure first
(cl-clojure-syntax:enable-clojure-syntax)

(let* ((input "\"42\"")
       (preprocessed (cl-clojure-syntax:preprocess-clojure-dots input)))
  (format t "Input: ~A~%" input)
  (format t "Preprocessed: ~A~%" preprocessed)
  
  (with-input-from-string (stream preprocessed)
    (let* ((read-result (cl-clojure-syntax:read-clojure stream))
           (convert-result (cl-clojure-syntax::convert-cl-quasiquote read-result)))
      (format t "Read result: ~A~%" read-result)
      (format t "Read type: ~A~%" (type-of read-result))
      (format t "Convert result: ~A~%" convert-result)
      (format t "Convert type: ~A~%" (type-of convert-result)))))
