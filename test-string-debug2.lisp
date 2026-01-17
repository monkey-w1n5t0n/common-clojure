#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")

(in-package #:cl-clojure-syntax)

;; Test read-clojure first
(let* ((input "\"42\"")
       (preprocessed (preprocess-clojure-dots input)))
  (format t "Input: ~A~%" input)
  (format t "Preprocessed: ~A~%" preprocessed)
  
  (with-input-from-string (stream preprocessed)
    (let* ((read-result (read-clojure stream))
           (convert-result (convert-cl-quasiquote read-result)))
      (format t "Read result: ~A~%" read-result)
      (format t "Read type: ~A~%" (type-of read-result))
      (format t "Convert result: ~A~%" convert-result)
      (format t "Convert type: ~A~%" (type-of convert-result)))))
