#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

(defun trace-eval-file-limit (path limit)
  "Evaluate forms in a Clojure file with debug trace, stopping after N forms or on error."
  (unless *current-env*
    (init-eval-system))
  (let* ((content (with-open-file (s path :direction :input)
                    (let ((str (make-string (file-length s))))
                      (read-sequence str s)
                      str)))
         (preprocessed (cl-clojure-syntax:preprocess-clojure-dots content))
         (results nil)
         (form-count 0)
         (comment-marker (get-comment-marker)))
    (with-input-from-string (stream preprocessed)
      (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
        (block :done
          (loop for form = (cl-clojure-syntax:read-clojure stream nil :eof)
                until (eq form :eof)
                unless (eq form comment-marker)
                  do (incf form-count)
                     (when (> form-count limit)
                       (format t "~&Stopping after ~d forms (limit)~%" limit)
                       (return-from :done (nreverse results)))
                     (format t "~&[~d] Form: ~A~%" form-count form)
                     (let ((result (handler-case
                                       (clojure-eval form *current-env*)
                                     (error (c)
                                       (format t "~&    ERROR: ~A~%" c)
                                       (format t "~&    Type: ~A~%" (type-of c))
                                       (return-from :done (nreverse results))))))
                       (format t "~&    Result: ~A~%" result)
                       (push result results)))))
    (nreverse results)))

(format t "Testing array_symbols with limited trace...~%")
(trace-eval-file-limit "clojure-tests/array_symbols.clj" 5)
