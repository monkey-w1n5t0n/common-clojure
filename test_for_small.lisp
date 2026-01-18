(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

(in-package :cl-clojure-eval)

;; Initialize the eval system
(init-eval-system)

;; Test small for comprehension
(format t "~&=== Testing small for comprehension ===~%")
(let ((path (make-pathname :name "test_for_small" :type "clj"
                           :defaults (make-pathname :directory '(:relative ".")))))
  (with-open-file (s path :direction :input)
    (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
      (loop for form = (cl-clojure-syntax:read-clojure s nil :eof)
            until (eq form :eof)
            do (let ((result (clojure-eval form *current-env*)))
                 (format t "Result: ~a~%" result))))))
