#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Read just the deftest form
(let* ((content (with-open-file (s "clojure-tests/array_symbols.clj" :direction :input)
                   (let ((str (make-string (file-length s))))
                     (read-sequence str s)
                     str)))
       (preprocessed (cl-clojure-syntax:preprocess-clojure-dots content))
       (comment-marker (get-comment-marker))
       (form-count 0))
  (with-input-from-string (stream preprocessed)
    (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
      (loop
        (let ((form (cl-clojure-syntax:read-clojure stream nil :eof)))
          (when (eq form :eof)
            (return))
          (unless (eq form comment-marker)
            (incf form-count)
            (when (= form-count 3)
              (format t "~&Form ~d: ~A~%" form-count form)
              (format t "Type: ~A~%" (type-of form))
              (format t "Length: ~A~%" (length form))
              (format t "cadr: ~A~%" (cadr form))
              (format t "cddr length: ~A~%" (length (cddr form)))
              (format t "caddr (first body): ~A~%" (caddr form))
              (return))))))))
