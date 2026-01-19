#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Test set! directly
(format t "Testing set! directly...~%")

;; First check what the form looks like
(let ((form-str "(set! *warn-on-reflection* true)"))
  (let* ((preprocessed (cl-clojure-syntax:preprocess-clojure-dots form-str)))
    (with-input-from-string (stream preprocessed)
      (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
        (let ((form (cl-clojure-syntax:read-clojure stream)))
          (format t "Form: ~A~%" form)
          (format t "Length: ~A~%" (length form))
          (format t "cadr: ~A~%" (cadr form))
          (format t "caddr: ~A~%" (caddr form))
          (format t "cddr: ~A~%" (cddr form))

          (handler-case
              (progn
                (format t "~&Evaluating...~%")
                (let ((result (clojure-eval form *current-env*)))
                  (format t "Result: ~A~%" result)))
            (error (c)
              (format t "~&ERROR: ~A~%" c)
              (format t "Type: ~A~%" (type-of c))
              (sb-debug:backtrace))))))))
