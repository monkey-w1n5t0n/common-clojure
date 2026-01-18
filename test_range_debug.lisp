(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

(in-package :cl-clojure-eval)

;; Initialize the eval system
(init-eval-system)

;; Test range directly
(format t "~&=== Testing clojure-range directly ===~%")
(format t "clojure-range: ~a~%" (cl-clojure-eval::clojure-range 3))
(format t "clojure-range (0 args): ~a~%" (cl-clojure-eval::clojure-range))
(format t "clojure-range (2 args): ~a~%" (cl-clojure-eval::clojure-range 1 5))

;; Test range via eval
(format t "~&=== Testing range via eval ===~%")
(let ((path (make-pathname :name "test_range" :type "clj"
                           :defaults (make-pathname :directory '(:relative ".")))))
  (with-open-file (s path :direction :input)
    (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
      (loop for form = (cl-clojure-syntax:read-clojure s nil :eof)
            until (eq form :eof)
            do (let ((result (clojure-eval form *current-env*)))
                 (format t "Result via eval: ~a~%" result))))))
