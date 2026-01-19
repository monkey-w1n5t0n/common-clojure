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
                 (require [clojure.test-helper :as util])) *current-env*)

;; Read and evaluate the deftest form manually
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
              (format t "~&Form ~d is (deftest ...)~%" form-count)
              (let ((name (cadr form))
                    (body (cddr form)))
                (format t "  Name: ~A~%" name)
                (format t "  Body has ~d forms~%" (length body))
                (format t "~&  Evaluating body forms one by one:~%")
                (let ((i 0))
                  (dolist (expr body)
                    (incf i)
                    (format t "  [~d] expr type: ~A, length: ~A~%"
                            i (type-of expr) (if (consp expr) (length expr) "n/a"))
                    (handler-case
                        (let ((result (clojure-eval expr *current-env*)))
                          (format t "      result: ~A~%" result))
                      (error (c)
                        (format t "      ERROR: ~A~%" c)
                        (return-from nil nil)))))
              (return))))))))
