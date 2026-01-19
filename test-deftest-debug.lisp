#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Setup ns
(clojure-eval '(ns clojure.test-clojure.array-symbols
                 (use clojure.test)
                 (require #(clojure.test-helper as util))) *current-env*)

;; Read the deftest form
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
              (format t "~&Form ~d: (deftest ...)~%" form-count)
              (let ((name (cadr form))
                    (body (cddr form)))
                (format t "  Name: ~A~%" name)
                (format t "  Body length: ~A~%" (length body))
                (format t "~&  Evaluating each body form...~%")
                (let ((i 0))
                  (dolist (expr body)
                    (incf i)
                    (format t "  [~d] expr: ~A..." i (if (and (consp expr) (> (length (prin1-to-string expr)) 100))
                                                        (subseq (prin1-to-string expr) 0 100)
                                                        expr))
                    (handler-case
                        (let ((result (clojure-eval expr *current-env*)))
                          (format t "  -> ~A~%" (if (and (consp result) (> (length (prin1-to-string result)) 50))
                                                    (subseq (prin1-to-string result) 0 50)
                                                    result)))
                      (error (c)
                        (format t "  -> ERROR: ~A~%" c)
                        (format t "  Type: ~A~%" (type-of c))
                        (return)))))
              (return))))))))
