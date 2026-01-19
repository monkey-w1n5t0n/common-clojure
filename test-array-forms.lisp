#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Evaluate each form from array_symbols.clj one by one
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
            (format t "~&[~d] Form: ~A~%" form-count form)
            (handler-case
                (let ((result (clojure-eval form *current-env*)))
                  (format t "~&    Result: ~A~%" (if (and (consp result) (> (length result) 20))
                                                   (subseq (prin1-to-string result) 0 100)
                                                   result)))
              (error (c)
                (format t "~&    ERROR: ~A~%" c)
                (format t "~&    Type: ~A~%" (type-of c))
                (return)))))))))
