#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Add debug trace to eval-file
(defun trace-eval-file (path)
  "Evaluate all forms in a Clojure file with debug trace."
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
        (loop for form = (cl-clojure-syntax:read-clojure stream nil :eof)
              until (eq form :eof)
              unless (eq form comment-marker)
                do (incf form-count)
                   (format t "~&[~d] Evaluating: ~A~%" form-count form)
                   (let ((result (handler-case
                                     (clojure-eval form *current-env*)
                                   (error (c)
                                     (format t "~&    ERROR: ~A~%" c)
                                     (format t "~&    Type: ~A~%" (type-of c))
                                     (signal 'error :format-control "Error in form")))))
                     (format t "~&    Result: ~A~%" result)
                     (push result results)))))
    (nreverse results)))

;; Try evaluating array_symbols forms
(format t "Testing array_symbols with trace...~%")

(handler-case
    (progn
      (format t "Loading clojure-tests/array_symbols.clj...~%")
      (let ((results (trace-eval-file "clojure-tests/array_symbols.clj")))
        (format t "~&Success! Completed ~d forms.~%" (length results))
        (format t "Last result: ~A~%" (car results))))
  (error (c)
    (format t "~&Fatal Error: ~A~%" c)))
